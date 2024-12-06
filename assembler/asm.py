
from srcfile import *
from asmerr import *
from tokenizer import *
from preproc import *
from asmexpr import *

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
pseudo_ops = [
    'mov'
]
mnemonics = alu_modes + branch_modes + pseudo_ops


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

class OperandType(Enum):
    MEM   = 0
    PTR   = 1
    ZPAGE = 2
    IMM   = 3

class Operand:
    def __init__(self, type: OperandType, value: SymRef):
        self.type  = type
        self.value = value

class Insn:
    def __init__(self, mnemonic: str, args: list[Operand]):
        self.mnemonic = mnemonic
        self.args     = args
    
    @staticmethod
    def parse(args: list[Token]) -> Self:
        pass


def is_mnemonic(tkn: Token) -> bool:
    if tkn.type != TokenType.IDENTIFIER: return False
    pass

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
            expr = parse_expr(args, equ, directive.loc)
            expr.assert_const()
            if expr.offset < addr: raise AsmError(f".org directive goes backwards", directive.loc.including(args[-1].loc))
            addr = expr.offset
            
        elif directive.val == '.zero':
            expr = parse_expr(args, equ, directive.loc)
            expr.assert_const()
            for i in range(expr.offset):
                write_byte(0, directive.loc)
            
        elif directive.val == '.byte':
            if len(args) < 1: raise AsmError("Expected an expression")
            while args:
                if Token(',', TokenType.OTHER) in args:
                    comma = args.index(Token(',', TokenType.OTHER))
                    expr, args = parse_expr(args[:comma], equ, args[0].loc), args[comma+1:]
                else:
                    expr, args = parse_expr(args, equ, directive.loc), []
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
                raise AsmError(f"Instruction {name} expects expression but got memory reference")
            amode = 1
            val   = parse_expr(args[1:-1], equ, args[1].loc)
        elif len(args) >= 4 and args[0].type == TokenType.IDENTIFIER and args[0].val in ['zp', 'zpage', 'zeropage']\
            and args[1] == Token('[', TokenType.OTHER) and args[-1] == Token(']', TokenType.OTHER):
            # Memory reference (explicitly zero-page).
            if not allow_mem:
                raise AsmError(f"Instruction {name} expects expression but got memory reference")
            amode = 0
            val   = parse_expr(args[2:-1], equ, args[2].loc)
        elif len(args) >= 3 and args[0] == Token('[', TokenType.OTHER) and args[-1] == Token(']', TokenType.OTHER):
            # Memory reference.
            if not allow_mem:
                raise AsmError(f"Instruction {name} expects expression but got memory reference")
            val   = parse_expr(args[1:-1], equ, args[1].loc)
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
            val   = parse_expr(args, equ, name_loc)
        
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
