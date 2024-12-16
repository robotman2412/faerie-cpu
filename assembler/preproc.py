
import os

from srcfile import *
from asmerr import *
from tokenizer import *

class MacroDef:
    def __init__(self, params: list[str], tokens: list[Token]):
        self.params = params
        self.tokens = tokens

class PragmaOnce:
    def __init__(self, path: str, line: int):
        self.realpath = os.path.realpath(path)
        self.line     = line
    
    def __eq__(self, other: Self):
        if type(other) != PragmaOnce:
            return False
        return self.realpath == other.realpath and self.line == other.line
    
    def __hash__(self):
        return hash(self.realpath)

class Preprocessor(Iterable[Token]):
    def __init__(self, tokens: Iterator[Token], inc_path: list[str] = ['.'], macros: dict[str,MacroDef] = {}, once: set[PragmaOnce] = set()):
        self.stack    = [tokens]
        self.inc_path = inc_path
        self.macros   = macros
        self.once     = once
    
    def find_include_file(self, name: str, relative_to: str = None) -> str:
        if relative_to:
            if os.path.isfile(os.path.dirname(relative_to) + '/' + name):
                return os.path.dirname(relative_to) + '/' + name
        for path in self.inc_path:
            if os.path.isfile(path + '/' + name):
                return path + '/' + name
        return None
    
    def _eat_eol(self):
        tkn: Token = next(self.stack[-1])
        while tkn != None and tkn.type != TokenType.NEWLINE:
            tkn = next(self.stack[-1])
    
    def _expect_eol(self):
        """Raise a warning if there are more tokens on the current line."""
        tkn: Token = next(self.stack[-1])
        if tkn == None: return
        loc = tkn.loc
        if tkn == None or tkn.type == TokenType.NEWLINE:
            return
        while tkn != None and tkn.type != TokenType.NEWLINE:
            tkn = next(self.stack[-1])
            if tkn != None:
                loc = loc.including(tkn.loc)
        raise_warn("Extra tokens ignored", loc)
    
    def _include(self, loc: Location):
        """Handle #include directives."""
        tkn: Token = next(self.stack[-1], None)
        if tkn == None or tkn.type != TokenType.STRING:
            raise_err("#include expects a filename", loc); return
        self._expect_eol()
        path = self.find_include_file(tkn.val, tkn.loc.file.path)
        if path == None:
            raise_err(f"Include file not found: {repr(tkn.val)}", tkn.loc); return
        if len(self.stack) > 100:
            loc = loc.including(tkn.loc)
            loc.inc_from = None
            raise_err("Include file recursion limit exceeded", loc); return
        try:
            with open(path, "r") as fd:
                file = SourceFile(fd.read(), tkn.val, path)
                self.stack.append(iter(Tokenizer(file, loc.including(tkn.loc))))
        except FileNotFoundError:
            raise_err(f"Include file not found: {repr(tkn.val)}"); return
    
    def _define(self, loc: Location):
        """Handle #define directives."""
        # Get an identifier to use as macro name.
        tkn: Token = next(self.stack[-1], None)
        if tkn == None:
            raise_err("#define expects an identifier", loc); return
        elif tkn.type != TokenType.IDENTIFIER or not is_sym_str(tkn.val, allow_period=False):
            raise_err("#define expects an identifier", tkn.loc); return
        macro_name = tkn
        
        # Get all remaining tokens on this line.
        tmp: list[Token] = []
        while True:
            tkn = next(self.stack[-1], None)
            if tkn == None or tkn.type == TokenType.NEWLINE: break
            # Add the macro name to the location.
            tmp.append(tkn.with_loc(tkn.loc.as_macro(macro_name)))
        
        params: list[str] = []
        if tmp[0] == Token('(', TokenType.OTHER) and tmp[0].loc.off == macro_name.loc.off + len(macro_name.val):
            # Parameterized macro; get parameters list.
            tmp = tmp[1:]
            if len(tmp) and tmp[0] == Token(')', TokenType.OTHER):
                tmp = tmp[1:]
            else:
                while True:
                    if len(tmp) < 2:
                        raise_err("Expected `)`", loc); return
                    elif tmp[0].type != TokenType.IDENTIFIER or not is_sym_str(tmp[0].val, allow_period=False):
                        raise_err("Macro parameter expects an identifier", tmp[0].loc)
                    params.append(tmp[0].val)
                    if tmp[1] == Token(',', TokenType.OTHER):
                        tmp = tmp[2:]
                    elif tmp[1] == Token(')', TokenType.OTHER):
                        tmp = tmp[2:]; break
                    else:
                        raise_err("Expected `)` or `,`", tmp[1].loc); return
        
        if macro_name.val in self.macros:
            # Warn on redefinition.
            raise_warn(f"Redefinition of {macro_name.val}", loc)
        self.macros[macro_name.val] = MacroDef(params, tmp)
    
    def _undef(self, loc: Location):
        """Handle #define directives."""
        # Get an identifier to use as macro name.
        tkn: Token = next(self.stack[-1], None)
        if tkn == None:
            raise_err("#undef expects an identifier", loc); return
        elif tkn.type != TokenType.IDENTIFIER or not is_sym_str(tkn.val, allow_period=False):
            raise_err("#undef expects an identifier", tkn.loc); return
        if tkn.val not in self.macros:
            raise_warn("#undef of non-existant macro", tkn.loc)
        self._expect_eol()
    
    def _pragma(self, loc: Location):
        tkn: Token = next(self.stack[-1], None)
        if tkn.type != TokenType.IDENTIFIER:
            raise_err("Expected an identifier", tkn.loc)
            self._eat_eol()
            return
        elif tkn.val != "once":
            raise_err("Unknown pragma", tkn.loc)
            self._eat_eol()
            return
        self._expect_eol()
        pragma_once = PragmaOnce(tkn.loc.file.path, tkn.loc.line)
        if pragma_once in self.once:
            while next(self.stack[-1], None) != None:
                pass
        else:
            self.once.add(pragma_once)
    
    def _expand_macro(self, macro_name: Token) -> tuple[Token|None, list[Token]]:
        """Expand a macro and output all its tokens."""
        def recursive_expand(tokens: list[Token], no_expand: list[str]) -> list[Token]:
            """Recursively expand macros."""
            out = []
            while len(tokens):
                if tokens[0].type == TokenType.IDENTIFIER and tokens[0].val not in no_expand and tokens[0].val in self.macros:
                    if not self.macros[tokens[0].val].params:
                        # Recursively expand unparameterized macro.
                        out.extend(expand_noparam(tokens[0], no_expand))
                        tokens = tokens[1:]
                    elif len(tokens) == 1 or tokens[1] != Token('(', TokenType.OTHER):
                        # Parameterized macro without params; don't expand.
                        out.append(tokens[0])
                        tokens = tokens[1:]
                    else:
                        # Parse params for macro.
                        params: list[list[Token]] = [[]]
                        i = 2
                        paren = 1
                        while True:
                            if i >= len(tokens):
                                raise AsmError("Missing `)` in macro expansion", tokens[0].loc.including(tokens[-1].loc))
                            elif tokens[i] == Token(',', TokenType.OTHER):
                                params.append([])
                            elif tokens[i] == Token(')', TokenType.OTHER):
                                paren -= 1
                                if paren == 0: break
                                params[-1].append(tokens[i])
                            elif tokens[i] == Token('(', TokenType.OTHER):
                                paren += 1
                                params[-1].append(tokens[i])
                            else:
                                params[-1].append(tokens[i])
                            i += 1
                        out.extend(expand_param(tokens[0], params, tokens[0].loc.including(tokens[i].loc), no_expand))
                        tokens = tokens[i+1:]
                else:
                    out.append(tokens[0])
                    tokens = tokens[1:]
            return out
        
        def expand_noparam(macro_name: Token, no_expand: list[str] = []) -> list[Token]:
            """Expand an unparameterized macro."""
            out = []
            for exp in self.macros[macro_name.val].tokens:
                out.append(exp.with_loc(macro_name.loc.with_exp_from(exp.loc)))
            return recursive_expand(out, no_expand + [macro_name.val])
        
        def expand_param(macro_name: Token, params: list[list[Token]], loc: Location, no_expand: list[str] = []) -> list[Token]:
            """Expand a parameterized macro."""
            macro = self.macros[macro_name.val]
            if len(macro.params) != len(params) and (len(macro.params) != 0 or params != [[]]):
                raise AsmError(f"Macro `{macro_name.val}` expects {len(macro.params)} args but got {len(params)} args", loc)
            params_map = {macro.params[i]: params[i] for i in range(len(macro.params))}
            out = []
            for exp in macro.tokens:
                if exp.type == TokenType.IDENTIFIER and exp.val in params_map:
                    out.extend(x.with_loc(x.loc.with_exp_from(exp.loc)) for x in params_map[exp.val])
                else:
                    out.append(exp.with_loc(macro_name.loc.with_exp_from(exp.loc)))
            return recursive_expand(out, no_expand + [macro_name.val])
        
        try:
            if not self.macros[macro_name.val].params:
                # Macro is unparameterized; don't try to parse params for it.
                tmp = expand_noparam(macro_name)
                for x in tmp: x.loc.macro_inst = macro_name.loc
                return None, 
            
            tkn = next(self.stack[-1], None)
            if tkn != Token('(', TokenType.OTHER) or tkn != None and tkn.loc.off != macro_name.loc.off + len(macro_name.val):
                # Macro not expanded.
                return tkn, [macro_name]
            
            # Grab params for this macro.
            tmp = [tkn]
            paren = 1
            while paren:
                tkn = next(self.stack[-1], None)
                if tkn == None:
                    raise AsmError("Missing `)` in macro expansion", macro_name.loc.including(tkn[-1].loc))
                elif tkn == Token(')', TokenType.OTHER):
                    paren -= 1
                elif tkn == Token('(', TokenType.OTHER):
                    paren += 1
                tmp.append(tkn)
            
            # Parse params for this macro.
            params: list[list[Token]] = [[]]
            i = 1
            while i < len(tmp) - 1:
                if tmp[i] == Token(',', TokenType.OTHER):
                    params.append([])
                else:
                    params[-1].append(tmp[i])
                i += 1
            
            # Finally perform macro expansion.
            tmp = expand_param(macro_name, params, macro_name.loc.including(tmp[-1].loc))
            for x in tmp: x.loc.macro_inst = macro_name.loc
            return None, tmp
            
        except AsmError as e:
            # Error during macro expansion.
            raise_err(e.args[0], e.loc)
            self._eat_eol()
            return None, []
    
    def __iter__(self):
        tkn = None
        while len(self.stack):
            if tkn == None:
                pass
            elif tkn == Token('#', TokenType.OTHER):
                # Preproc directives.
                directive: Token = next(self.stack[-1], None)
                if directive == None:
                    yield tkn
                elif directive.type != TokenType.IDENTIFIER:
                    raise_err("Expected preprocessor directive", directive.loc)
                    self._eat_eol()
                else:
                    loc = tkn.loc.including(directive.loc)
                    match directive.val:
                        case "include": self._include(loc)
                        case "define":  self._define(loc)
                        case "undef":   self._undef(loc)
                        case "pragma":  self._pragma(loc)
                        case _:
                            raise_err(f"Unknown directive {directive.val}")
                            self._eat_eol()
            elif tkn.type == TokenType.IDENTIFIER and tkn.val in self.macros:
                # Replace defines.
                tkn, exp = self._expand_macro(tkn)
                for x in exp: yield x
                if tkn: continue
            else:
                # Some normal token.
                yield tkn
            
            # Get next token in current file.
            tkn: Token = next(self.stack[-1], None)
            if tkn == None:
                # File is empty, pop it off the stack.
                self.stack.pop()
                continue
