
from typing import *
from typing_extensions import Self

from srcfile import *
from asmerr import *

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
