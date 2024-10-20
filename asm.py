#!/usr/bin/env python3

# The Faerie CPU assembler.
# Yes, it could be more efficient for large files.
# No, it does not matter; this CPU has a mere 16 bits of address space.

from enum import Enum
from typing import *
from typing_extensions import Self
import argparse, sys, traceback

alu_modes = [
    'shl',  'xor',  'add',  'or',
    'shr',  'ld',   'sub',  'and',
    'shlc', 'xorc', 'addc', 'orc',
    'shrc', 'ldc',  'subc', 'andc'
]
branch_modes = [
    'beq', 'bcs', 'bgt', 'blt',
    'bne', 'bcc', 'ble', 'bge',
    'j'
]


class Location:
    def __init__(self, filename: str, off: int, line: int, col: int, len: int = 1):
        self.filename = filename
        self.off      = off
        self.line     = line
        self.col      = col
        self.len      = len
    
    def __str__(self):
        return f"{self.filename}:{self.line}:{self.col} ({self.len} char{'s' if self.len != 1 else ''})"
    
    def __repr__(self):
        return f"Location({repr(self.filename)}, {self.off}, {self.line}, {self.col}, {self.len})"
    
    def with_len(self, len: int):
        return Location(self.filename, self.off, self.line, self.col, len)
    
    def including(self, other: Self):
        if other.off < self.off:
            return other.with_len(max(other.len, self.off - other.off + self.len))
        else:
            return self.with_len(max(self.len, other.off - self.off + other.len))

class AsmError(Exception):
    def __init__(self, msg: str, loc: Location = None):
        Exception.__init__(self, msg)
        self.loc = loc

class StringLocIterator:
    def __init__(self, raw: Iterator[str], filename: str = "<anonymous>"):
        self.raw      = raw
        self.buf      = None
        self.filename = filename
        self.off      = 0
        self.line     = 1
        self.col      = 1
        
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
            self.buf = None
            return '\n'
        else:
            self.off += 1
            self.col += 1
            tmp, self.buf = self.buf, None
            return tmp
    
    def loc(self):
        return Location(self.filename, self.off, self.line, self.col)

class TokenType(Enum):
    CONSTANT   = 0
    IDENTIFIER = 1
    STRING     = 2
    CHAR       = 3
    OTHER      = 4
    NEWLINE    = 5

class Token:
    def __init__(self, val: str|int, type: TokenType, loc: Location = None):
        self.val  = val
        self.type = type
        self.loc  = loc
    
    def __str__(self):
        return f"{str(self.loc)} {self.type.name}({repr(self.val)})"
    
    def __repr__(self):
        return f"Token({repr(self.val)}, {self.type}, {repr(self.loc)})"
    
    def __eq__(self, other: Self):
        return other is self or (type(other) == Token and other.val == self.val and other.type == self.type)

class Tokenizer:
    def __init__(self, raw: Iterable[str], filename: str = "<anonymous>"):
        self.raw      = raw
        self.filename = filename
    
    def __iter__(self):
        long   = ['<<', '>>', '<=', '>=', '==', '!=', '&&', '||']
        stream = StringLocIterator(iter(self.raw), self.filename)
        loc    = stream.loc()
        char   = next(stream, None)
        
        while char:
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
                    yield Token(int(tmp, 0), TokenType.CONSTANT, loc.with_len(len(tmp)))
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
                    raise AsmError(f"{"String" if term == '"' else "Character"} constant spans end of {"line" if char else "file"}", loc)
                
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
                            raise AsmError("Block comment spans end of file", loc)
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
                
            elif char == '\n':
                # Newlines.
                yield Token(None, TokenType.NEWLINE, loc)
                
            elif char > ' ':
                # Other characters.
                yield Token(char, TokenType.OTHER, loc)
            
            loc  = stream.loc()
            char = next(stream, None)

def is_sym_char(char: str, allow_numeric = True) -> bool:
    n = ord(char)
    if ord('a') <= n <= ord('z') or ord('A') <= n <= ord('Z'):
        return True
    elif allow_numeric and ord('0') <= n <= ord('9'):
        return True
    else:
        return char in ['.', '_', '$']

def is_sym_str(sym: str) -> bool:
    if not is_sym_char(sym[0], False): return False
    for char in sym[1:]:
        if not is_sym_char(char): return False
    return True


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


class SymRef:
    def __init__(self, offset: int = 0, symbol: str = None, loc: Location = None):
        self.offset = offset
        self.symbol = symbol
        self.loc    = loc
    
    def assert_const(self):
        if self.symbol != None:
            raise AsmError("Expected constant, got symbol reference")
    
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
            raise AsmError(f"`{args[i]}` not expected here", args[i].loc)
    
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
            if type(args[i]) == Token and args[x].val == '(': depth += 1
            if type(args[i]) == Token and args[x].val == ')': depth -= 1
            x += 1
        args = args[:i] + [parse_expr(args[i+1:x-1])] + args[x:]
        i += 1
    
    # Pass 2: Collapse prefix operators.
    i = len(args)-1
    while i > 0:
        if type(args[i]) != str and args[i-1] in unary and (i == 1 or type(args[i-2]) == str):
            if args[i-1] != '+': args[i].assert_const()
            args = args[:i-1] + [SymRef(unary[args[i-1]](args[i].offset), None, args[i-1].loc.including(args[i].loc))] + args[i+1:]
        i -= 1
    
    # Pass 3: Binary operators.
    if type(args[0]) == Token:
        raise AsmError(f"`{args[0]}` not expected here")
    for oper in binary:
        i = 0
        while i < len(args)-2:
            if type(args[i+1]) != str: raise AsmError(f"`{args[i+1]}` not expected here")
            if type(args[i+2]) == str: raise AsmError(f"`{args[i+2]}` not expected here")
            if args[i+1] in oper:
                # Enforce exprs to be additive w.r.t. symbols.
                if args[i+1] != '+':
                    args[i+2].assert_const()
                    if args[i+1] != '-':
                        args[i].assert_const()
                # Calculate the constant expr.
                if args[i].symbol and args[i+2].symbol:
                    raise AsmError("Can't add two symbols to each other")
                symbol = args[i].symbol or args[i+2].symbol
                args = args[:i] + [SymRef(oper[args[i+1]](args[i].offset, args[i+2].offset), symbol, args[i].loc.including(args[i+2].loc))] + args[i+3:]
            else:
                i += 2
    
    if type(args[0]) != SymRef:
        raise AsmError(f"`{args[0]}` not expected here")
    return args[0]


class RelocType(Enum):
    MEMLO = 0
    MEMHI = 1
    ZPAGE = 2
    ZPPTR = 3
    BYTE  = 4

class Reloc:
    def __init__(self, addr: int, sym: SymRef, type: RelocType, loc: Location=None):
        self.addr = addr
        self.sym  = sym
        self.type = type
        self.loc  = loc


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


class NewlineFilter:
    def __init__(self, raw):
        self.raw = raw
    
    def __iter__(self):
        prev = None
        for char in self.raw:
            if char == '\r':
                yield '\n'
            elif char == '\n':
                if prev != '\r': yield '\n'
            else:
                yield char
            prev = char

def preprocess(raw: str) -> list[tuple[str,int]]:
    lines: list[tuple[str,int]] = []
    linenum = 1
    line = ""
    
    iterator = iter(NewlineFilter(raw))
    char = next(iterator, None)
    while char:
        if char == '/':
            char = next(iterator, None)
            if char == None: break
            if char == '/': # Line comment.
                while char and char != '\n':
                    char = next(iterator, None)
                continue
            elif char == '*': # Block comment.
                prev = None
                while char:
                    if char == '\n':
                        if line: lines.append((line, linenum))
                        line = ""
                        linenum += 1
                    if prev == '*' and char == '/': break
                    prev, char = char, next(iterator, None)
        elif char == '\n':
            if line: lines.append((line, linenum))
            line = ""
            linenum += 1
        else:
            line += char
        char = next(iterator, None)
    
    return lines

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
    
    def write_symref(ref: SymRef, type: RelocType):
        nonlocal out, reloc, addr, a2l
        if addr > 65535: raise AsmError("Address overflow")
        a2l[addr] = ref.loc
        reloc.append(Reloc(addr, ref, type, ref.loc))
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
                raise AsmError(f"Instruction {name} expression but got memory reference")
            val   = parse_expr(args[1:-1], equ)
            if val.symbol or val.offset & 0x100:
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
            write_symref(val, RelocType.ZPAGE)
        elif amode == 1:
            write_symref(val, RelocType.ZPPTR)
        elif amode == 2:
            write_symref(val, RelocType.MEMLO)
            write_symref(val, RelocType.MEMHI)
        else:
            write_symref(val, RelocType.BYTE)
    
    
    # Pass 1: Write data and reloc entries, find labels.
    line: list[Token] = []
    for token in raw:
        if token.type != TokenType.NEWLINE:
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
                raise AsmError(*e.args, line[0].loc.including(line[-1].loc))
            else:
                raise e
        except Exception as e:
            eclass, exc, trace = sys.exc_info()
            msg = "Traceback (most recent call last):\n"
            for x in traceback.format_tb(trace):
                msg += x
            msg += "Exception while parsing: "+e.args[0]
            raise AsmError(msg, line[0].loc.including(line[-1].loc))
        line = []
    
    # Pass 2: Apply relocations.
    for rel in reloc:
        if rel.sym.symbol:
            if rel.sym.symbol not in symbols:
                raise AsmError(f"Undefined reference to `{rel.sym.symbol}`", rel.loc)
            relval = symbols[rel.sym.symbol] + rel.sym.offset
        else:
            relval = rel.sym.offset
        
        # Check relocation constraints.
        match rel.type:
            case RelocType.ZPAGE:
                if relval < 0 or relval > 255:
                    raise AsmError(f"Zero-page address {relval} out of range (0-255)", rel.loc)
            case RelocType.ZPPTR:
                if relval < 0 or relval > 255:
                    raise AsmError(f"Zero-page address {relval} out of range (0-255)", rel.loc)
                if relval & 1:
                    raise AsmError(f"Zero-page pointer address {relval} misaligned (should be 16-bit aligned)", rel.loc)
            case RelocType.MEMLO:
                if relval < 0 or relval > 65535:
                    raise AsmError(f"Address {relval} out of range (0-65535)", rel.loc)
            case RelocType.MEMHI:
                if relval < 0 or relval > 65535:
                    raise AsmError(f"Address {relval} out of range (0-65535)", rel.loc)
                relval >>= 8
            case RelocType.BYTE:
                if relval < -128 or relval > 255:
                    print(f"Warning {rel.loc.filename}:{rel.loc.line}:{rel.loc.col}: Value truncated from {relval} to {relval & 255}")
        
        # Update the byte.
        out[rel.addr] = relval & 255
    
    # Post-process: Turn into array.
    start, end = min(out.keys()), max(out.keys())
    blob = []
    for addr in range(start, end+1):
        if addr in out:
            blob.append(out[addr])
        else:
            blob.append(0)
    
    return blob, a2l, symbols


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Assembler for the Faerie CPU\nhttps://github.com/robotman2412/faerie-cpu")
    parser.add_argument("-o", "--outfile", action="store", default="out.lhf")
    parser.add_argument("--format", "-O",  action="store", choices=["binary", "logisim"], default="logisim")
    parser.add_argument("infile",          action="store")
    args = parser.parse_args()
    
    with open(args.infile, "r") as fd:
        try:
            out, a2l, symbols = assemble(Tokenizer(fd.read(), args.infile))
        except AsmError as e:
            print(f"Error {e.loc.filename}:{e.loc.line}:{e.loc.col}: {e.args[0]}")
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
