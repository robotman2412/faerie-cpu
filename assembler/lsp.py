
import sys, os, json, time

from asm import *

debug: TextIO = None

def db_print(*args, sep=' ', end='\n'):
    global debug
    if debug == None: return
    for i in range(len(args)):
        if i: debug.write(sep)
        debug.write(str(args[i]))
    debug.write(end)
    debug.flush()

def await_msg() -> tuple[str,int,dict]:
    headers = {}
    while True:
        line = sys.stdin.readline()
        if line == '' or line == None:
            time.sleep(0.01)
            continue
        line = line.strip()
        if not line: break
        idx = line.index(':')
        headers[line[:idx]] = line[idx+1:].strip()
    content_length = int(headers['Content-Length'])
    raw_data = sys.stdin.read(content_length)
    obj = json.JSONDecoder().decode(raw_data)
    assert 'jsonrpc' in obj and 'id' in obj and 'method' in obj and 'params' in obj
    assert type(obj['jsonrpc']) == str and 2 <= float(obj['jsonrpc']) < 3
    assert type(obj['method']) == str
    assert type(obj['params']) == dict
    return obj['method'], obj['id'], obj['params']

def send_resp_msg(id: int, result: Any):
    obj = {"jsonrpc": "2.0", "id": id, "result": result}
    raw_data = json.JSONEncoder().encode(obj)
    sys.stdout.write(f"Content-Length: {len(raw_data)}\r\n\r\n")
    sys.stdout.write(raw_data)
    sys.stdout.flush()

def send_err_msg(id: int, err_code: int, err_str: str):
    obj = {"jsonrpc": "2.0", "id": id, "error": {"code": err_code, "message": err_str or f"Error {err_code}"}}
    raw_data = json.JSONEncoder().encode(obj)
    sys.stdout.write(f"Content-Length: {len(raw_data)}\r\n\r\n")
    sys.stdout.write(raw_data)
    sys.stdout.flush()

def send_notif_msg(method: str, params: dict|list|None):
    obj = {"jsonrpc": "2.0", "method": method}
    if params != None: obj['params'] = params
    raw_data = json.JSONEncoder().encode(obj)
    sys.stdout.write(f"Content-Length: {len(raw_data)}\r\n\r\n")
    sys.stdout.write(raw_data)
    sys.stdout.flush()


def uri_to_file_path(uri: str) -> str:
    if not uri.startswith('file://'): return None
    return uri[7:]

def file_path_to_uri(path: str) -> str:
    path = os.path.abspath(path)
    if os.name == 'nt':
        path = path.replace('\\', '/')
    return 'file://' + path

def loc_to_lsp_range(loc: Location) -> dict:
    end = loc.end_location()
    return {
        'start': {
            'line':      loc.line-1,
            'character': loc.col-1
        },
        'end': {
            'line':      end.line-1,
            'character': end.col-1
        }
    }

lsp_severity_map = [4, 2, 1]


def lsp_do_document_diagnostic(id: int, uri: str):
    path = uri_to_file_path(uri)
    if not path:
        send_err_msg(id, -32602, f"Cannot convert {uri} to a path")
        return
    
    # Assemble the file to get all errors within.
    db_print(f"Assembling {path} for diagnostics...")
    clear_msg_log()
    try:
        with open(path, "r") as fd:
            srcfile = SourceFile(fd.read(), os.path.basename(path), path)
            assemble_srcfile(srcfile)
    except FileNotFoundError:
        send_err_msg(id, -32803, f"File not found: {path}")
        return
    db_print("Done!")
    
    # Convert messages to the LSP diagnostic format.
    diagnostics: dict[str,list] = {}
    for type, msg, loc in get_msg_log():
        uri = file_path_to_uri(loc.file.path)
        if uri not in diagnostics:
            diagnostics[uri] = []
        related = []
        diagnostics[uri].append({
            'range':              loc_to_lsp_range(loc),
            'severity':           lsp_severity_map[type.value],
            'message':            msg,
            'relatedInformation': related
        })
        if loc.exp_from:
            related.append({
                'location': {
                    'uri': file_path_to_uri(loc.exp_from.file.path),
                    'range': loc_to_lsp_range(loc.exp_from),
                },
                'message': f"Expanded from macro {loc.exp_from.macro}"
            })
    
    # Format diagnosic message.
    related = {}
    param = {
        'kind':             'full',
        'items':            diagnostics.pop(file_path_to_uri(path), []),
        'relatedDocuments': related,
    }
    for uri in diagnostics.keys():
        related[uri] = {
            'kind':  'full',
            'ietms': diagnostics[uri]
        }
    
    send_resp_msg(id, param)

def lsp_do_document_hover(id: int, uri: str, lsp_range: dict):
    path = uri_to_file_path(uri)
    if not path:
        send_err_msg(id, -32602, f"Cannot convert {uri} to a path")
        return
    
    # Preprocess the entire file.
    try:
        with open(path, "r") as fd:
            srcfile = SourceFile(fd.read(), os.path.basename(path), path)
            tokens = preprocess_srcfile(srcfile)
    except FileNotFoundError:
        send_err_msg(id, -32803, f"File not found: {path}")
        return
    
    # Iterate tokens looking for the specified line and col.
    lsp_line = lsp_range['line'] + 1
    lsp_col  = lsp_range['character'] + 1
    
    def loc_matches(loc: Location) -> bool:
        if loc.macro_inst: loc = loc.macro_inst
        if loc.file.path != path: return False
        if loc.line > lsp_line: return False
        if loc.col > lsp_col: return False
        end = loc.end_location()
        if end.line < lsp_line: return False
        if end.col < lsp_col: return False
        return True
    
    matches: Token = None
    macro_matches: list[Token] = []
    i = len(tokens)-1
    while i >= 0 and not loc_matches(tokens[i].loc):
        i -= 1
    while loc_matches(tokens[i].loc):
        if tokens[i].loc.exp_from:
            macro_matches.append(tokens[i])
        else:
            matches = tokens[i]
            break
        i -= 1
    macro_matches.reverse()
    
    # Provide information for found tokens.
    if macro_matches:
        loc = macro_matches[0].loc.exp_from
        while loc.exp_from:
            loc = loc.exp_from
        markdown  = f"Expanded from macro [`{loc.macro}`]"
        markdown += f"({file_path_to_uri(loc.file.path)}#L{loc.line})"
        markdown += f" defined in {loc.file.name}\n"
        markdown += '```faerieasm\n'
        markdown += ' '.join(str(token) for token in macro_matches)
        markdown += '\n```'
        params = {
            'contents': {
                'kind': 'markdown',
                'value': markdown
            },
            'range': loc_to_lsp_range(macro_matches[0].loc),
        }
    elif matches:
        params = {
            'contents': {'kind': 'markdown', 'value': ''},
            'range':    loc_to_lsp_range(matches.loc),
        }
    else:
        params = None
    
    send_resp_msg(id, params)


lsp_caps = {
    'hoverProvider': True,
    # 'completionProvider': {
    #     'triggerCharacters': '.'
    # },
    'diagnosticProvider': {
        'interFileDependencies': True,
        'workspaceDiagnostics':  False,
    },
}

def lsp_main():
    db_print("Waiting for initialize request")
    initialized = False
    while True:
        try: method, id, params = await_msg()
        except: continue
        if not initialized:
            if method == 'initialize':
                initialized = True
                send_resp_msg(id, {'capabilities': lsp_caps})
                db_print("Initialized")
            else:
                db_print("Message before LSP was initialized")
                send_err_msg(id, -32002, "LSP not initialized")
            continue
        
        match method:
            case 'initialize':
                db_print("LSP was already initialized")
                send_err_msg(id, -32600, "LSP was already initialized")
            case 'textDocument/diagnostic':
                lsp_do_document_diagnostic(id, params['textDocument']['uri'])
            case 'textDocument/hover':
                lsp_do_document_hover(id, params['textDocument']['uri'], params['position'])
            case _:
                db_print(f"Method {method} not supported")
                db_print(json.JSONEncoder().encode(params))
                send_err_msg(id, -32601, f"Method {method} not supported")
