#!/usr/bin/env python3

# The Faerie CPU assembler.
# Yes, it could be more efficient for large files.
# No, it does not matter; this CPU has a mere 16 bits of address space.

from enum import Enum
from typing import *
from typing_extensions import Self
import argparse, sys, os, json, time, traceback

alu_modes = [
    'shl',  'xor',  'add',  'or',
    'shr',  'ld',   'sub',  'and',
    'shlc', 'xorc', 'addc', 'orc',
    'shrc', 'ldc',  'subc', 'andc'
]
branch_modes = [
    'beq', 'bcs', 'bgt', 'blt',
    'bne', 'bcc', 'ble', 'bge',
    'j',   'jsr'
]


def split_lines(raw: str) -> list[str]:
    out = [""]
    while len(raw):
        if raw[0] == '\r':
            if len(raw) > 1 and raw[1] == '\n':
                raw = raw[2:]
            else:
                raw = raw[1:]
            out.append("")
        elif raw[0] == '\n':
            raw = raw[1:]
            out.append("")
        else:
            out[-1] += raw[0]
            raw      = raw[1:]
    return out

class SourceFile:
    def __init__(self, content: str, name: str = None, path: str = None):
        self.content = content
        self.name    = name or "<anonymous>"
        self.path    = path or self.name

class Location:
    def __init__(self, \
            file: SourceFile, off: int, line: int, col: int, len: int = 1, \
            inc_from: Self = None, exp_from: Self = None, macro: str = None):
        self.file     = file
        self.off      = off
        self.line     = line
        self.col      = col
        self.len      = len
        self.inc_from = inc_from
        self.exp_from = exp_from
        self.macro    = macro
    
    def __str__(self):
        return f"{self.file.name}:{self.line}:{self.col}"
    
    def __repr__(self):
        return f"Location(file <{repr(self.file.name)}>, {self.off}, {self.line}, {self.col}, {self.len}, {repr(self.inc_from)}, {repr(self.exp_from)}, {repr(self.macro)})"
    
    def show(self):
        # Find the start of the line before this location.
        start_off = self.off
        while self.file.content[start_off] not in ['\r', '\n']:
            start_off -= 1
        start_off += 1
        
        # Find the end of the line after this location.
        end_off = self.off + self.len
        while self.file.content[end_off] not in ['\r', '\n']:
            end_off += 1
        
        # Print all lines of the location individually.
        lines = split_lines(self.file.content[start_off:end_off])
        if len(lines) > 1:
            print(f"{self.line:4d} | {lines[0]}")
            print("     | " + ' ' * (self.col-1) + '^' + '~' * (len(lines[0])-self.col))
            for i in range(1, len(lines)-1):
                print(f"{self.line+i:4d} | {lines[i]}")
                print("     | " + '~' * len(lines[i]))
            print(f"{self.line:4d} | {lines[-1]}")
            print("     | " + '~' * (len(lines[-1]) - end_off + self.off + self.len))
        else:
            print(f"{self.line:4d} | {lines[0]}")
            print("     | " + ' ' * (self.col-1) + '^' + '~' * (self.len-1))
    
    def copy(self) -> Self:
        return Location(self.file, self.off, self.line, self.col, self.len, self.inc_from, self.exp_from, self.macro)
    
    def end_location(self) -> Self:
        new = self.copy()
        while new.len:
            new.len -= 1
            if new.file.content[new.off] == '\r':
                if new.file.content[new.off + 1] == '\n':
                    new.off += 1
                new.line += 1
                new.col   = 1
            elif new.file.content[new.off] == '\n':
                new.line += 1
                new.col   = 1
            else:
                new.col  += 1
            new.off += 1
        return new
    
    def as_macro(self, macro_name: str) -> Self:
        new = self.copy()
        new.macro = macro_name
        return new
    
    def with_inc_from(self, inc_from: Self) -> Self:
        new = self.copy()
        new.inc_from = inc_from
        return new
    
    def with_exp_from(self, exp_from: Self) -> Self:
        new = self.copy()
        if new.exp_from:
            exp_from = exp_from.copy()
            exp_from.exp_from = new.exp_from
            new.exp_from = exp_from
        else:
            new.exp_from = exp_from
        return new
    
    def with_len(self, len: int) -> Self:
        new = self.copy()
        new.len = len
        return new
    
    def including(self, other: Self) -> Self:
        if other.off < self.off:
            tmp = other.with_len(max(other.len, self.off - other.off + self.len))
        else:
            tmp = self.with_len(max(self.len, other.off - self.off + other.len))
        if self.exp_from and other.exp_from and self.exp_from.macro == other.exp_from.macro:
            tmp.exp_from = self.exp_from.including(other.exp_from)
        return tmp

class Severity(Enum):
    HINT  = 0
    WARN  = 1
    ERROR = 2

msg_name = ["Hint", "Warning", "Error"]
msg_col  = ["\033[35m", "\033[33m", "\033[31m"]
no_col   = "\033[0m"

def print_msg(type: Severity, msg: str, loc: Location = None):
    def _print(type: Severity, msg: str, loc_str: str):
        print(msg_col[type.value] + msg_name[type.value] + " " + loc_str + ": " + msg + no_col)
    if not loc:
        _print(type, msg, "<anonymous>:?")
        return
    
    # Show message location.
    _print(type, msg, str(loc))
    loc.show()
    link = loc
    while link.inc_from:
        _print(Severity.HINT, f"Included from {link.file.name}", str(link.inc_from))
        link.inc_from.show()
        link = link.inc_from
    
    # Show macro it was expanded from.
    file = loc.file
    while loc.exp_from:
        _print(Severity.HINT, f"Expanded from macro {loc.exp_from.macro}", str(loc.exp_from))
        loc.exp_from.show()
        link = loc.exp_from
        while link.inc_from and link.file is not file:
            _print(Severity.HINT, f"Included from {link.file.name}", str(link.inc_from))
            link.inc_from.show()
            link = link.inc_from
        loc = loc.exp_from

_msg_log: list[tuple[Severity,str,Location]] = []

def get_msg_log():
    return _msg_log

def clear_msg_log():
    global _msg_log
    _msg_log = []

def log_msg(type: Severity, msg: str, loc: Location = None):
    _msg_log.append((type, msg, loc))


_on_msg_handler = print_msg
_has_errors = False

def raise_msg(type: Severity, msg: str, loc: Location = None):
    _on_msg_handler(type, msg, loc)

def raise_warn(msg: str, loc: Location = None):
    raise_msg(Severity.WARN, msg, loc)

def raise_err(msg: str, loc: Location = None):
    global _has_errors
    _has_errors = True
    raise_msg(Severity.ERROR, msg, loc)

class AsmError(Exception):
    def __init__(self, msg: str, loc: Location = None):
        Exception.__init__(self, msg)
        self.loc = loc



class StringLocIterator(Iterator[str]):
    def __init__(self, raw: Iterator[str], file: SourceFile, inc_from: Location = None):
        self.raw      = raw
        self.buf      = None
        self.file     = file
        self.off      = 0
        self.line     = 1
        self.col      = 1
        self.inc_from = inc_from
        
    def __next__(self):
        if not self.buf:
            self.buf = next(self.raw)
        if self.buf == '\r':
            char = next(self.raw, None)
            self.off  += 1
            self.line += 1
            self.col   = 1
            if char != '\n':
                self.buf = char
            else:
                self.buf = None
                self.off += 1
            return '\n'
        elif self.buf == '\n':
            self.off  += 1
            self.line += 1
            self.col   = 1
            self.buf   = None
            return '\n'
        else:
            self.off += 1
            self.col += 1
            tmp, self.buf = self.buf, None
            return tmp
    
    def loc(self):
        return Location(self.file, self.off, self.line, self.col, inc_from=self.inc_from)

class TokenType(Enum):
    CONSTANT   = 0
    IDENTIFIER = 1
    STRING     = 2
    CHAR       = 3
    OTHER      = 4
    NEWLINE    = 5
    ERROR      = 6

class Token:
    def __init__(self, val: str|int, type: TokenType, loc: Location = None):
        self.val  = val
        self.type = type
        self.loc  = loc
    
    def __str__(self):
        if self.type == TokenType.CHAR:
            return "'" + self.val.encode("string_escape") + "'"
        elif self.type == TokenType.STRING:
            return '"' + self.val.encode("string_escape") + '"'
        else:
            return str(self.val)
    
    def __repr__(self):
        return f"Token({repr(self.val)}, {self.type}, {repr(self.loc)})"
    
    def __eq__(self, other: Self):
        return other is self or (type(other) == Token and other.val == self.val and other.type == self.type)
    
    def with_loc(self, loc: Location):
        return Token(self.val, self.type, loc)

class Tokenizer(Iterable[Token]):
    def __init__(self, file: SourceFile, inc_from: Location = None):
        self.file     = file
        self.inc_from = inc_from
    
    def __iter__(self):
        long   = ['<<', '>>', '<=', '>=', '==', '!=', '&&', '||']
        stream = StringLocIterator(iter(self.file.content), self.file, self.inc_from)
        loc    = stream.loc()
        char   = next(stream, None)
        
        while char:
            try:
                if is_sym_char(char, True):
                    # Identifiers and numeric constants.
                    tmp      = char
                    next_loc = stream.loc()
                    char     = next(stream, None)
                    while char and is_sym_char(char, True):
                        tmp += char
                        next_loc = stream.loc()
                        char = next(stream, None)
                    if tmp[0] in '0123456789':
                        try:
                            yield Token(int(tmp, 0), TokenType.CONSTANT, loc.with_len(len(tmp)))
                        except ValueError as e:
                            raise_err(e.args[0], loc.with_len(len(tmp)))
                            yield Token(tmp, TokenType.ERROR, loc.with_len(len(tmp)))
                    else:
                        yield Token(tmp, TokenType.IDENTIFIER, loc.with_len(len(tmp)))
                    loc = next_loc
                    continue
                    
                elif char == '\'' or char == '\"':
                    # String and char consts.
                    term     = char
                    char     = next(stream, None)
                    tmp      = ""
                    
                    def eol():
                        nonlocal char, term
                        raise_err(f"{"String" if term == '"' else "Character"} constant spans end of {"line" if char else "file"}", loc)
                        yield Token(None, TokenType.ERROR, loc)
                    
                    def num_esc(base: int, len: int, raw: str = ""):
                        nonlocal esc_loc
                        for _ in range(len):
                            char = next(stream, None)
                            if not char or char == '\n': raise AsmError("Invalid escape sequence", esc_loc)
                            raw += char
                            esc_loc.len += 1
                        try:
                            return chr(int(raw, base))
                        except ValueError:
                            raise AsmError("Invalid escape sequence", esc_loc)
                    
                    while True:
                        if not char or char == '\n': eol()
                        loc.len += 1
                        if char == '\\':
                            esc_loc = stream.loc()
                            char    = next(stream, None)
                            match char:
                                case None, '\n': eol()
                                case 'a':  tmp += '\a'
                                case 'b':  tmp += '\b'
                                case 'f':  tmp += '\f'
                                case 'n':  tmp += '\n'
                                case 'r':  tmp += '\r'
                                case 't':  tmp += '\t'
                                case 'v':  tmp += '\v'
                                case '\\': tmp += '\\'
                                case '\'': tmp += '\''
                                case '\"': tmp += '\"'
                                case '0', '1', '2', '3': tmp += num_esc(8, 2, char)
                                case 'x': tmp += num_esc(16, 2)
                                case 'u': tmp += num_esc(16, 4)
                                case 'U': tmp += num_esc(16, 8)
                                case _: raise AsmError(f"Invalid escape sequence", esc_loc)
                        elif char == term:
                            if term == '"':
                                yield Token(tmp, TokenType.STRING, loc)
                            elif len(tmp) == 0:
                                raise AsmError("Empty character constant", loc)
                            elif len(tmp) > 1:
                                raise AsmError("Multiple characters in character constant", loc)
                            else:
                                yield Token(ord(tmp), TokenType.CONSTANT, loc)
                            break
                        else:
                            tmp += char
                        char     = next(stream, None)
                    
                elif char == '/':
                    prev     = char
                    next_loc = stream.loc()
                    char     = next(stream, None)
                    if char == '/':
                        # Line comment.
                        loc  = stream.loc()
                        char = next(stream, None)
                        while char:
                            if char == '\n': break
                            loc  = stream.loc()
                            char = next(stream, None)
                        yield Token(None, TokenType.NEWLINE, loc)
                    elif char == '*':
                        # Block comment.
                        char = next(stream, None)
                        prev = None
                        while True:
                            if not char:
                                raise_err("Block comment spans end of file", loc)
                            if prev == '*' and char == '/': break
                            prev = char
                            char = next(stream, None)
                    else:
                        # Just a forward slash.
                        yield Token('/', TokenType.OTHER, loc)
                        loc = next_loc
                        continue
                    
                elif char in '<>=!&|':
                    # Long operators.
                    prev = char
                    next_loc = stream.loc()
                    char = next(stream, None)
                    if (prev+char) in long:
                        yield Token(prev+char, TokenType.OTHER, loc.with_len(2))
                    else:
                        yield Token(prev, TokenType.OTHER, loc)
                        loc = next_loc
                        continue
                    
                elif char == '%':
                    # Modulo, %hi or %lo.
                    tmp      = ""
                    next_loc = stream.loc()
                    id_loc   = next_loc
                    char     = next(stream, None)
                    while char and is_sym_char(char, True):
                        tmp += char
                        next_loc = stream.loc()
                        char = next(stream, None)
                    if tmp == 'hi' or tmp == 'lo':
                        yield Token('%' + tmp, TokenType.OTHER, loc.with_len(len(tmp)+1))
                        continue
                    else:
                        if tmp:
                            yield Token(tmp, TokenType.IDENTIFIER, id_loc.with_len(len(tmp)))
                        yield Token('%', TokenType.OTHER, loc)
                        loc = next_loc
                        continue
                    
                elif char == '\n':
                    # Newlines.
                    yield Token(None, TokenType.NEWLINE, loc)
                    
                elif char > ' ':
                    # Other characters.
                    yield Token(char, TokenType.OTHER, loc)
                
                loc  = stream.loc()
                char = next(stream, None)
            
            except AsmError as e:
                # On error, consume blindly 'til EOL.
                yield Token(None, TokenType.ERROR, loc)
                while char != '\n':
                    char = next(stream, None)
                loc  = stream.loc()
                char = next(stream, None)

def is_sym_char(char: str, allow_numeric = True, allow_period = True) -> bool:
    n = ord(char)
    if ord('a') <= n <= ord('z') or ord('A') <= n <= ord('Z'):
        return True
    elif allow_numeric and ord('0') <= n <= ord('9'):
        return True
    elif allow_period and char == '.':
        return True
    else:
        return char == '_' or char == '$'

def is_sym_str(sym: str, allow_period = True) -> bool:
    if not is_sym_char(sym[0], False, allow_period): return False
    for char in sym[1:]:
        if not is_sym_char(char, True, allow_period): return False
    return True


class MacroDef:
    def __init__(self, params: list[str], tokens: list[Token]):
        self.params = params
        self.tokens = tokens

class Preprocessor(Iterable[Token]):
    def __init__(self, tokens: Iterator[Token], inc_path: list[str] = ['.'], macros: dict[str,MacroDef] = {}):
        self.stack    = [tokens]
        self.inc_path = inc_path
        self.macros   = macros
    
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
                return None, expand_noparam(macro_name)
            
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
            return None, expand_param(macro_name, params, macro_name.loc.including(tmp[-1].loc))
            
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
                


class SymRefType(Enum):
    DEFAULT = 0
    HIGH    = 1
    LOW     = 2

class SymRef:
    def __init__(self, offset: int = 0, symbol: str = None, loc: Location = None, type = SymRefType.DEFAULT):
        self.offset = offset
        self.symbol = symbol
        self.loc    = loc
        self.type   = type
    
    def assert_const(self):
        if self.symbol != None:
            raise AsmError("Expected constant, got symbol reference", self.loc)
        elif self.type == SymRefType.HIGH:
            raise AsmError("%hi is not supported in constant expressions", self.loc)
        elif self.type == SymRefType.LOW:
            raise AsmError("%lo is not supported in constant expressions", self.loc)
    
    def __repr__(self):
        return f"SymRef({self.offset}, {repr(self.symbol)})"
    
    def __str__(self):
        if self.symbol and self.offset:
            return f"{self.symbol}+{self.offset}"
        elif self.symbol:
            return f"{self.symbol}"
        else:
            return f"{self.offset}"

def parse_expr(args: list[Token|SymRef], equ: dict[str,int] = {}) -> SymRef:
    # Could I do this with an LR parser? Yes.
    # Will I convert this into an LR parser? No.
    if len(args) == 0:
        raise ValueError("Empty expr")
    
    args = args.copy()
    # Define operators.
    unary = {
        '-': lambda a: -a,
        '+': lambda a: a,
        '!': lambda a: not a,
        '~': lambda a: ~a,
    }
    unary_only = ['!', '~']
    binary = [
        {
            '*':  lambda a, b: a * b,
            '/':  lambda a, b: a // b,
            '%':  lambda a, b: a % b
        }, {
            '-':  lambda a, b: a - b,
            '+':  lambda a, b: a + b
        }, {
            '<<': lambda a, b: a << b,
            '>>': lambda a, b: a >> b
        }, {
            '<=': lambda a, b: a <= b,
            '>=': lambda a, b: a >= b,
            '>':  lambda a, b: a > b,
            '<':  lambda a, b: a < b
        }, {
            '==': lambda a, b: a == b,
            '!=': lambda a, b: a != b
        }, {
            '&':  lambda a, b: a & b
        }, {
            '^':  lambda a, b: a ^ b
        }, {
            '|':  lambda a, b: a | b
        }, {
            '&&': lambda a, b: a and b
        }, {
            '||': lambda a, b: a or b
        }
    ]
    valid_op = unary_only.copy()
    for set in binary:
        valid_op.extend(set.keys())
    valid_op.extend(['%hi', '%lo'])
    
    # Pass 0: Convert stuff to Relocation.
    for i in range(len(args)):
        if type(args[i]) == SymRef:
            pass
        elif args[i].type == TokenType.IDENTIFIER:
            if args[i].val in equ:
                args[i] = SymRef(equ[args[i].val], None, args[i].loc)
            else:
                args[i] = SymRef(0, args[i].val, args[i].loc)
        elif args[i].type == TokenType.CONSTANT:
            args[i] = SymRef(args[i].val, None, args[i].loc)
        elif args[i].val not in valid_op and args[i].val not in '()':
            raise AsmError(f"{args[i]} not expected here", args[i].loc)
    
    # Pass 1: Recursively evaluate parenthesized exprs.
    i = 0
    while i < len(args):
        if type(args[i]) == Token and args[i].val == ')':
            raise AsmError("Unmatched closing parenthesis", args[i].loc)
        if args[i] != Token('(', TokenType.OTHER):
            i += 1
            continue
        x     = i + 1
        depth = 1
        while depth:
            if x >= len(args):
                raise AsmError("Unmatched opening parenthesis", args[i].loc)
            if type(args[x]) == Token and args[x].val == '(': depth += 1
            if type(args[x]) == Token and args[x].val == ')': depth -= 1
            x += 1
        args = args[:i] + [parse_expr(args[i+1:x-1])] + args[x:]
        i += 1
    
    # Pass 2: Collapse prefix operators.
    i = len(args)-1
    while i > 0:
        if type(args[i]) == SymRef and args[i-1] == Token('%hi', TokenType.OTHER):
            args = args[:i-1] + [SymRef(args[i].offset, args[i].symbol, args[i-1].loc.including(args[i].loc), SymRefType.HIGH)] + args[i+1:]
        elif type(args[i]) == SymRef and args[i-1] == Token('%lo', TokenType.OTHER):
            args = args[:i-1] + [SymRef(args[i].offset, args[i].symbol, args[i-1].loc.including(args[i].loc), SymRefType.LOW)] + args[i+1:]
        elif type(args[i]) == SymRef and type(args[i-1]) == Token and args[i-1].type == TokenType.OTHER and args[i-1].val in unary \
            and (i <= 1 or args[i-1].val in unary_only or type(args[i-2]) == Token and args[i-2].type == TokenType.OTHER):
            if args[i-1] != '+': args[i].assert_const()
            args = args[:i-1] + [SymRef(unary[args[i-1].val](args[i].offset), None, args[i-1].loc.including(args[i].loc))] + args[i+1:]
        i -= 1
    
    # Pass 3: Binary operators.
    if type(args[0]) == Token:
        raise AsmError(f"{args[0]} not expected here", args[0].loc)
    for oper in binary:
        i = 0
        while i < len(args)-2:
            if type(args[i+1]) != Token or args[i+1].type != TokenType.OTHER:
                raise AsmError(f"{args[i+1]} not expected here", args[i+1].loc)
            if type(args[i+2]) == Token:
                raise AsmError(f"{args[i+2]} not expected here", args[i+2].loc)
            if args[i+1].val in oper:
                # Enforce exprs to be additive w.r.t. symbols.
                if args[i+1] != '+':
                    args[i+2].assert_const()
                    if args[i+1] != '-':
                        args[i].assert_const()
                # Calculate the constant expr.
                if args[i].symbol and args[i+2].symbol:
                    raise AsmError("Can't add two symbols to each other", args[i].loc.including(args[i+2].loc))
                symbol = args[i].symbol or args[i+2].symbol
                args = args[:i] + [SymRef(oper[args[i+1].val](args[i].offset, args[i+2].offset), symbol, args[i].loc.including(args[i+2].loc))] + args[i+3:]
            else:
                i += 2
    
    if type(args[0]) != SymRef:
        raise AsmError(f"{args[0]} not expected here", args[0].loc)
    return args[0]


class RelocType(Enum):
    MEMLO = 0
    MEMHI = 1
    ZPAGE = 2
    ZPPTR = 3
    BYTE  = 4

class Reloc:
    def __init__(self, addr: int, sym: SymRef, type: RelocType, loc: Location=None, is_write: bool=False):
        self.addr     = addr
        self.sym      = sym
        self.type     = type
        self.loc      = loc
        self.is_write = is_write


def an(s):
    return f'an {s}' if s[0] in 'aeiou' else f'a {s}'

def expect(actual, expected):
    if actual != expected: raise AsmError(f"Expected `{actual}`, got `{expected}`")
    return actual

def explabel(obj: Token):
    if obj.type != TokenType.IDENTIFIER:
        raise AsmError(f"Expected identifier, got `{obj}`")
    return obj

def listexpect(list, index, expected):
    try:
        return expect(list[index], expected)
    except IndexError:
        raise AsmError(f"Expected `{expected}`")

def listexplabel(list, index):
    try:
        return explabel(list[index])
    except IndexError:
        raise AsmError("Expected identifier")


def assemble(raw: Iterable[Token]) -> tuple[list[int], dict[int,Location], dict[str,int]]:
    symbols: dict[str,int] = {}
    equ:     dict[str,int] = {}
    reloc:   list[Reloc]   = []
    out:     dict[int,int] = {}
    a2l:     dict[int,Location] = {}
    addr = 0
    
    
    def write_byte(value: int, loc: Location):
        nonlocal out, addr, a2l
        if addr > 65535: raise AsmError("Address overflow")
        a2l[addr] = loc
        out[addr] = value
        addr += 1
    
    def write_symref(ref: SymRef, type: RelocType, is_write: bool = False):
        nonlocal out, reloc, addr, a2l
        if addr > 65535: raise AsmError("Address overflow")
        a2l[addr] = ref.loc
        if type == RelocType.BYTE and ref.type == SymRefType.HIGH:
            type = RelocType.MEMHI
        elif type == RelocType.BYTE and ref.type == SymRefType.LOW:
            type = RelocType.MEMLO
        elif ref.type == SymRefType.HIGH:
            raise AsmError("%hi not allowed here", ref.loc)
        elif ref.type == SymRefType.LOW:
            raise AsmError("%lo not allowed here", ref.loc)
        reloc.append(Reloc(addr, ref, type, ref.loc, is_write))
        addr += 1
    
    def handle_label(label: str):
        nonlocal symbols, equ, addr
        if label in equ:
            raise AsmError(f"Label redefines equation `{label}`")
        if label in symbols:
            raise AsmError(f"Redefinition of {label}")
        symbols[label] = addr
    
    def handle_directive(directive: Token, args: list[Token]):
        nonlocal equ, symbols, addr
        if directive.val == '.equ':
            listexplabel(args, 0)
            listexpect(args, 1, Token(',', TokenType.OTHER))
            if len(args) < 3: raise AsmError("Expected an expression")
            if args[0].val in symbols: raise AsmError(f"Equation redefines label `{args[0].val}`", directive.loc.including(args[0].loc))
            expr = parse_expr(args[2:], equ)
            expr.assert_const()
            equ[args[0].val] = expr.offset
            
        elif directive.val == '.org':
            if len(args) < 1: raise AsmError("Expected an expression")
            expr = parse_expr(args, equ)
            expr.assert_const()
            if expr.offset < addr: raise AsmError(f".org directive goes backwards", directive.loc.including(args[-1].loc))
            addr = expr.offset
            
        elif directive.val == '.zero':
            if len(args) < 1: raise AsmError("Expected an expression")
            expr = parse_expr(args, equ)
            expr.assert_const()
            for i in range(expr.offset):
                write_byte(0, directive.loc)
            
        elif directive.val == '.byte':
            if len(args) < 1: raise AsmError("Expected an expression")
            while args:
                if Token(',', TokenType.OTHER) in args:
                    comma = args.index(Token(',', TokenType.OTHER))
                    if comma == 0: raise AsmError("Expected an expression", args[comma].loc)
                    expr, args = parse_expr(args[:comma]), args[comma+1:]
                else:
                    expr, args = parse_expr(args), []
                write_symref(expr, RelocType.BYTE)
            
        elif directive.val in ['.ascii', '.asciz']:
            zterm = directive.val[-1] == 'z'
            if len(args) != 1 or args[0].type != TokenType.STRING:
                raise AsmError("Expected a string")
            for byte in args[0].val.encode():
                write_byte(byte, args[0].loc)
            if zterm:
                write_byte(0, args[0].loc)
            
        else:
            raise AsmError(f"Uknown directive `{directive.val}`")
    
    def handle_insn(args: list[Token]):
        nonlocal equ, symbols, reloc
        name_loc = args[0].loc
        name, args = args[0].val.lower(), args[1:]
        if name == 'cmp': name = 'sub.cmp'
        
        # Classify opcode and mode.
        allow_imm = True
        allow_mem = True
        if name == 'ret':
            # Return from subroutine.
            name = 'j'
            args = [Token('(', TokenType.OTHER), Token(0, TokenType.CONSTANT), Token(')', TokenType.OTHER)]
        if name == 'li':
            # Load immediate.
            has_args  = True
            allow_mem = False
            mode      = 5
            opcode    = 0
        elif name in alu_modes:
            # ALU ops.
            has_args  = name[:2] != 'sh'
            mode      = alu_modes.index(name)
            opcode    = 0
            allow_imm = name[:2] != 'ld'
        elif name.endswith('.cmp') and name[:-4] in alu_modes:
            # ALU ops (compare mode).
            has_args  = name[:2] != 'sh'
            mode      = alu_modes.index(name[:-4])
            opcode    = 1
            allow_imm = name[:2] != 'ld'
        elif name == 'st':
            # Store ops.
            has_args = True
            mode      = 0
            opcode    = 2
            allow_imm = False
        elif name in branch_modes:
            # Branch ops.
            has_args  = True
            mode      = branch_modes.index(name)
            opcode    = 3
            allow_imm = False
        else:
            raise AsmError(f"Unknown instruction {name}")
        
        # Parse addressing mode and arguments.
        if not has_args and not args:
            # No args.
            amode = 0
        elif not has_args:
            # Instruction has no args but something was supplied anyway.
            raise AsmError(f"Instruction {name} does not take arguments")
        elif has_args and not args:
            # Instruction has args but nothing was supplied.
            if allow_imm and allow_mem:
                raise AsmError(f"Instruction {name} expects memory reference or expression")
            elif allow_mem:
                raise AsmError(f"Instruction {name} expects memory reference")
            else:
                raise AsmError(f"Instruction {name} expects expression")
        elif len(args) >= 3 and args[0] == Token('(', TokenType.OTHER) and args[-1] == Token(')', TokenType.OTHER):
            # Zero-page pointer.
            if not allow_mem:
                raise AsmError(f"Instruction {name} expression but got memory reference")
            amode = 1
            val   = parse_expr(args[1:-1], equ)
        elif len(args) >= 4 and args[0].type == TokenType.IDENTIFIER and args[0].val in ['zp', 'zpage', 'zeropage']\
            and args[1] == Token('[', TokenType.OTHER) and args[-1] == Token(']', TokenType.OTHER):
            # Memory reference (explicitly zero-page).
            if not allow_mem:
                raise AsmError(f"Instruction {name} expression but got memory reference")
            amode = 0
            val   = parse_expr(args[2:-1], equ)
        elif len(args) >= 3 and args[0] == Token('[', TokenType.OTHER) and args[-1] == Token(']', TokenType.OTHER):
            # Memory reference.
            if not allow_mem:
                raise AsmError(f"Instruction {name} expects expression but got memory reference")
            val   = parse_expr(args[1:-1], equ)
            if val.symbol or val.offset & 0xff00:
                amode = 2 # Need 16-bit address
            else:
                amode = 0 # Relaxed to 0-page address
        elif not allow_imm:
            # Instruction does not allow imm but it was supplied anyway.
            raise AsmError(f"Instruction {name} expects memory reference but got expression")
        else:
            # Constant expression.
            amode = 3
            val   = parse_expr(args, equ)
        
        # Emit instruction.
        write_byte(opcode | (amode << 2) | (mode << 4), name_loc)
        if not has_args:
            pass
        elif amode == 0:
            write_symref(val, RelocType.ZPAGE, opcode == 2)
        elif amode == 1:
            write_symref(val, RelocType.ZPPTR, opcode == 2)
        elif amode == 2:
            write_symref(val, RelocType.MEMLO, opcode == 2)
            write_symref(val, RelocType.MEMHI)
        else:
            write_symref(val, RelocType.BYTE,  opcode == 2)
    
    
    # Pass 1: Write data and reloc entries, find labels.
    line: list[Token] = []
    for token in raw:
        if token.type != TokenType.NEWLINE and token != Token(';', TokenType.OTHER):
            line.append(token)
            continue
        try:
            if len(line) >= 2 and line[0].type == TokenType.IDENTIFIER and line[1] == Token(':', TokenType.OTHER):
                handle_label(line[0].val)
                line = line[2:]
            if len(line) == 0: continue
            if line[0].type != TokenType.IDENTIFIER:
                raise AsmError("Expected instruction or directive", line[0].loc)
            elif line[0].val[0] == '.':
                handle_directive(line[0], line[1:])
            else:
                handle_insn(line)
        except AsmError as e:
            if not e.loc:
                raise_err(*e.args, line[0].loc.including(line[-1].loc))
            else:
                raise_err(*e.args, e.loc)
        line = []
    
    # Pass 2: Apply relocations.
    for rel in reloc:
        if rel.sym.symbol:
            if rel.sym.symbol not in symbols:
                raise_err(f"Undefined reference to `{rel.sym.symbol}`", rel.loc); continue
            relval = symbols[rel.sym.symbol] + rel.sym.offset
        else:
            relval = rel.sym.offset
        
        # Check relocation constraints.
        match rel.type:
            case RelocType.ZPAGE:
                if relval < 0 or relval > 255:
                    raise_err(f"Zero-page address {relval} out of range (0-255)", rel.loc); continue
            case RelocType.ZPPTR:
                if relval < 0 or relval > 255:
                    raise_err(f"Zero-page address {relval} out of range (0-255)", rel.loc); continue
                if relval & 1:
                    raise_err(f"Zero-page pointer address {relval} misaligned (should be 16-bit aligned)", rel.loc); continue
            case RelocType.MEMLO:
                if relval < 0 or relval > 65535:
                    raise_err(f"Address {relval} out of range (0-65535)", rel.loc); continue
            case RelocType.MEMHI:
                if relval < 0 or relval > 65535:
                    raise_err(f"Address {relval} out of range (0-65535)", rel.loc); continue
                relval >>= 8
            case RelocType.BYTE:
                if relval < -128 or relval > 255:
                    raise_warn(f"Value truncated from {relval} to {relval & 255}", rel.loc)
        
        # CPU MMIO registers are read-only.
        if rel.is_write and relval < 8:
            raise_warn(f"Write to read-only CPU MMIO register at address {relval}", rel.loc)
        
        # Update the byte.
        out[rel.addr] = relval & 255
    
    # Post-process: Turn into array.
    blob = []
    if len(out):
        start, end = min(out.keys()), max(out.keys())
        for addr in range(start, end+1):
            if addr in out:
                blob.append(out[addr])
            else:
                blob.append(0)
    
    return blob, a2l, symbols

def assemble_srcfile(file: SourceFile) -> tuple[list[int], dict[int,Location], dict[str,int]]:
    return assemble(Preprocessor(iter(Tokenizer(file))))

def preprocess_srcfile(file: SourceFile) -> list[Token]:
    return [x for x in Preprocessor(iter(Tokenizer(file)))]


def write_lhf(fd, data: list[int]):
    fd.write("v2.0 raw\n")
    for i in range(len(data)-1):
        fd.write(f"{data[i]:x} ")
    if data: fd.write(f"{data[-1]:x}\n")
    fd.flush()

def write_bin(fd, data: list[int], dlen: int):
    byte_count = (dlen + 7) // 8
    for word in data:
        for i in range(byte_count):
            fd.write((word >> (i*8)) & 255)



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



if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Assembler for the Faerie CPU\nhttps://github.com/robotman2412/faerie-cpu")
    parser.add_argument("--lsp",           action="store_true", help="Ignore all other arguments and start in language server mode")
    parser.add_argument("--stdio",         action="store_true", help=argparse.SUPPRESS)
    parser.add_argument("--lsp-debug",     action="store_true", help=argparse.SUPPRESS)
    parser.add_argument("--outfile", "-o", action="store", default="out.lhf")
    parser.add_argument("--format", "-O",  action="store", choices=["binary", "logisim"], default="logisim")
    parser.add_argument("infile",          action="store", nargs="?")
    args = parser.parse_args()
    
    if args.lsp:
        _on_msg_handler = log_msg
        if 0: # Set to 1 for debugging.
            fd = open("/home/julian/the_fpga/nanoproc/lsp.out", "a")
            sys.stderr = debug = fd
            sys.stdin.reconfigure(line_buffering = 0)
            try:
                lsp_main()
            except Exception as e:
                traceback.print_exc(file=debug)
                debug.flush()
                exit(1)
        else:
            try:
                lsp_main()
            except:
                exit(1)
        exit(0)
    
    out = a2l = symbols = None
    
    try:
        with open(args.infile, "r") as fd:
            file = SourceFile(fd.read(), os.path.basename(args.infile), args.infile)
            out, a2l, symbols = assemble_srcfile(file)
    except FileNotFoundError:
        print("File not found: " + args.infile)
        exit(1)
    
    if _has_errors:
        exit(1)
    
    if args.format == "logisim":
        with open(args.outfile, "w") as fd:
            write_lhf(fd, out)
    else:
        with open(args.outfile, "wb") as fd:
            write_bin(fd, out, 8)
    
    if len(symbols):
        print("Symbols:")
        for symname in symbols:
            print(f"  {symname:10} = 0x{symbols[symname]:x}")
    else:
        print("No symbols defined")
