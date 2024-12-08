
import re

from srcfile import *
from asmerr import *
from tokenizer import *
from preproc import *
from asmexpr import *


alu_modes = {
    'shl': 0,
    'xor': 1,
    'add': 2,
    'or':  3,
    'shr': 4,
    'sub': 6,
    'and': 7,
}
branch_modes = {
    'beq': 0,
    'bcs': 1,
    'bgt': 2,
    'blt': 3,
    'bne': 4,
    'bcc': 5,
    'ble': 6,
    'bge': 7,
    'j':   8,
    'jsr': 9,
}


class RelocType(Enum):
    MEMLO = 0
    MEMHI = 1
    ZPAGE = 2
    ZPPTR = 3
    BYTE  = 4
    BYTES = 5

class Reloc:
    def __init__(self, addr: int, sym: SymRef, type: RelocType, loc: Location=None, is_write: bool=False):
        self.addr     = addr
        self.sym      = sym
        self.type     = type
        self.loc      = loc
        self.is_write = is_write

class OperandType(Enum):
    # Zero-page memory reference.
    ZPAGE = 0
    # Pointer reference.
    PTR   = 1
    # Pointer reference that doesn't cross page boundaries.
    P_PTR = 2
    # Memory reference.
    MEM   = 3
    # Immediate value.
    IMM   = 4

class Operand:
    def __init__(self, type: OperandType, value: SymRef, loc: Location = None):
        self.type  = type
        self.value = value
        self.loc   = loc or self.value.loc
    
    def __repr__(self):
        return f"Operand({repr(self.type)}, {repr(self.value)}, {repr(self.loc)})"
    
    def __str__(self):
        match self.type:
            case OperandType.ZPAGE: return f"zpage [{self.value}]"
            case OperandType.MEM: return f"[{self.value}]"
            case OperandType.PTR: return f"({self.value})"
            case OperandType.P_PTR: return f"pwrap ({self.value})"
            case OperandType.IMM: return str(self.value)
    
    def next_byte(self) -> Self:
        new_value = SymRef(self.value.offset, self.value.symbol, self.value.loc, self.value.type)
        if self.type == OperandType.IMM:
            new_value.assert_const()
            new_value.offset >>= 8
        else:
            new_value.offset += 1
        return Operand(self.type, new_value, self.loc)
    
    def ptr2mem(self) -> Self:
        assert self.type == OperandType.PTR or self.type == OperandType.P_PTR
        new_value = SymRef(self.value.offset, self.value.symbol, self.value.loc, self.value.type)
        return Operand(OperandType.ZPAGE, new_value, self.loc)
    
    @staticmethod
    def parse(args: list[Token], equ: dict[str,int]) -> Self:
        if args[0].type == TokenType.IDENTIFIER and args[0].val.lower() in ['zp', 'zpage', 'zeropage']:
            args.pop(0)
            tkn = args.pop(0)
            if tkn != Token('[', TokenType.OTHER):
                raise AsmError("Expected [", tkn.loc)
            val = parse_expr(args[:-1], equ, tkn.loc)
            tkn = args.pop()
            if tkn != Token(']', TokenType.OTHER):
                raise AsmError("Expected ]", tkn.loc)
            return Operand(OperandType.ZPAGE, val)
            
        elif args[0] == Token('[', TokenType.OTHER):
            tkn = args.pop(0)
            val = parse_expr(args[:-1], equ, tkn.loc)
            tkn = args.pop()
            if tkn != Token(']', TokenType.OTHER):
                raise AsmError("Expected ]", tkn.loc)
            return Operand(OperandType.MEM, val)
            
        elif args[0].type == TokenType.IDENTIFIER and args[0].val.lower() in ['pw', 'pwrap', 'PAGEWRAP']:
            args.pop(0)
            tkn = args.pop(0)
            if tkn != Token('(', TokenType.OTHER):
                raise AsmError("Expected (", tkn.loc)
            val = parse_expr(args[:-1], equ, tkn.loc)
            tkn = args.pop()
            if tkn != Token(')', TokenType.OTHER):
                raise AsmError("Expected )", tkn.loc)
            return Operand(OperandType.P_PTR, val)
            
        elif args[0] == Token('(', TokenType.OTHER):
            tkn = args.pop(0)
            val = parse_expr(args[:-1], equ, tkn.loc)
            tkn = args.pop()
            if tkn != Token(')', TokenType.OTHER):
                raise AsmError("Expected )", tkn.loc)
            return Operand(OperandType.PTR, val)
            
        else:
            return Operand(OperandType.IMM, parse_expr(args))

class Insn:
    def __init__(self, mnemonic: str, args: list[Operand], loc: Location = None):
        self.mnemonic = mnemonic
        self.args     = args
        self.loc      = loc
    
    @staticmethod
    def parse(line: list[Token], equ: dict[str,int]) -> Self:
        loc                 = line[0].loc
        mnemonic: str       = line.pop(0).val.lower()
        comma               = Token(',', TokenType.OTHER)
        args: list[Operand] = []
        if line:
            while comma in line:
                idx = line.index(comma)
                args.append(Operand.parse(line[:idx], equ))
                if len(line) == idx - 1:
                    raise AsmError("Expected operand after ,", line[-1].loc)
                line = line[idx+1:]
            args.append(Operand.parse(line, equ))
        return Insn(mnemonic, args, loc)


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
    
    def encode_insn(opcode: int, mode: int, arg: Operand|None, loc: Location, is_repeat: bool = False):
        assert opcode >= 0 and opcode <= 3
        assert mode >= 0 and mode <= 15
        if opcode == 2:
            assert arg.type != OperandType.IMM
        if opcode <= 1 and mode & 3 == 0:
            assert not arg
            amode = 3
        else:
            assert arg
            match arg.type:
                case OperandType.ZPAGE: amode = 0
                case OperandType.PTR:   amode = 1
                case OperandType.P_PTR: amode = 1
                case OperandType.MEM:
                    arg.type = OperandType.ZPAGE
                    if not arg.value.symbol and arg.value.offset <= 255:
                        amode = 0
                    else:
                        amode = 2
                case OperandType.IMM:   amode = 3
        
        write_byte((mode << 4) | (amode << 2) | opcode, loc)
        if opcode <= 1 and mode & 3 == 0:
            return
        match arg.type:
            case OperandType.ZPAGE:
                write_symref(arg.value, RelocType.ZPAGE, opcode == 2)
            case OperandType.P_PTR:
                write_symref(arg.value, RelocType.ZPPTR, opcode == 2)
            case OperandType.PTR:
                write_symref(arg.value, RelocType.ZPPTR, opcode == 2)
            case OperandType.MEM:
                write_symref(arg.value, RelocType.MEMLO, opcode == 2)
                write_symref(arg.value, RelocType.MEMHI, opcode == 2)
            case OperandType.IMM:
                write_symref(arg.value, RelocType.BYTES if is_repeat else RelocType.BYTE, opcode == 2)
    
    def build_pseudo_op(args: list[Operand], calc_mode: int|None, cmp_mode: bool, r: range, loc: Location):
        if calc_mode == None:
            if len(args) < 2:
                raise AsmError("Expected memory reference or pointer, value", loc)
            elif len(args) > 2:
                raise AsmError("Too many operands", args[2].loc.including(args[-1].loc))
            elif args[0].type == OperandType.IMM:
                raise AsmError("Expected memory reference or pointer", args[0].loc)
        else:
            if len(args) < 2:
                raise AsmError("Expected memory reference, value", loc)
            elif len(args) > 2:
                raise AsmError("Too many operands", args[2].loc.including(args[-1].loc))
            elif args[0].type == OperandType.IMM:
                raise AsmError("Expected memory reference or pointer", args[0].loc)
            elif (args[0].type == OperandType.P_PTR or args[0].type == OperandType.PTR) and len(r) > 1:
                raise AsmError("Multiple-byte computation with pointers is unsupported")
            elif (args[1].type == OperandType.P_PTR or args[1].type == OperandType.PTR) and len(r) > 1:
                raise AsmError("Multiple-byte computation with pointers is unsupported")
        
        def next_byte(arg: Operand):
            if arg.type == OperandType.PTR:
                ptr = arg.ptr2mem()
                encode_insn(0, 5, ptr, loc)
                encode_insn(0, 2, Operand(OperandType.IMM, SymRef(r.step)), loc)
                encode_insn(2, 0, ptr, loc)
                ptr = ptr.next_byte()
                encode_insn(0, 5, ptr, loc)
                encode_insn(0, 10, Operand(OperandType.IMM, SymRef()), loc)
                encode_insn(2, 0, ptr, loc)
                return arg
            elif arg.type == OperandType.P_PTR:
                ptr = arg.ptr2mem()
                encode_insn(0, 5, ptr, loc)
                encode_insn(0, 2, Operand(OperandType.IMM, SymRef(r.step)), loc)
                encode_insn(2, 0, ptr, loc)
                return arg
            else:
                return arg.next_byte()
        
        for i in range(len(r)):
            if calc_mode == None:
                # Load instruction (mov edition).
                encode_insn(0, 5, args[1], loc, True)
            else:
                # Load instruction.
                encode_insn(0, 5, args[0], loc)
                if calc_mode & 3 == 0:
                    # Shift instruction.
                    encode_insn(+cmp_mode, calc_mode, None, loc)
                else:
                    # Other ALU instruction.
                    encode_insn(+cmp_mode, calc_mode, args[1], loc, True)
                # Enable flag carry mode after first instruction.
                calc_mode |= 8
            # Store instruction.
            if not cmp_mode:
                encode_insn(2, 0, args[0], loc, True)
            # Cycle to next arg byte.
            if i < len(r) - 1:
                for x in range(len(args)):
                    args[x] = next_byte(args[x])
        
        # Undo offsets to pointers.
        if len(r) < 2: return
        for arg in args:
            if arg.type == OperandType.PTR:
                ptr = arg.ptr2mem()
                encode_insn(0, 5, ptr, loc)
                encode_insn(0, 2, Operand(OperandType.IMM, SymRef(-r.step * (len(r)-1))), loc)
                encode_insn(2, 0, ptr, loc)
                ptr = ptr.next_byte()
                encode_insn(0, 5, ptr, loc)
                encode_insn(0, 10, Operand(OperandType.IMM, SymRef()), loc)
                encode_insn(2, 0, ptr, loc)
            elif arg.type == OperandType.P_PTR:
                ptr = arg.ptr2mem()
                encode_insn(0, 5, ptr, loc)
                encode_insn(0, 2, Operand(OperandType.IMM, SymRef(-r.step * (len(r)-1))), loc)
                encode_insn(2, 0, ptr, loc)
    
    def handle_insn(insn: Insn):
        # Get repetition number from insn.
        matches: re.Match = re.match(f"^({'|'.join(alu_modes.keys())}|mov)([248])+(\\.cmp)?$", insn.mnemonic)
        if matches:
            mnemonic = matches.group(1) + (matches.group(3) or "")
            repeat   = int(matches.group(2))
        else:
            mnemonic = insn.mnemonic
            repeat   = 1
        
        if mnemonic == 'cmp':
            mnemonic = 'sub'
            cmp_mode = True
        elif mnemonic.endswith(".cmp"):
            mnemonic = mnemonic[:-4]
            cmp_mode = True
        else:
            cmp_mode = False
        
        # Pseudo-ops.
        if mnemonic == 'mov':
            build_pseudo_op(insn.args, None, False, range(repeat), insn.loc)
            return
        elif mnemonic == 'shr' and (insn.args or repeat > 1):
            build_pseudo_op(insn.args, alu_modes[mnemonic], cmp_mode, range(repeat - 1, -1, -1), insn.loc)
            return
        elif mnemonic in alu_modes and (len(insn.args) > 1 or repeat > 1):
            build_pseudo_op(insn.args, alu_modes[mnemonic], cmp_mode, range(repeat), insn.loc)
            return
        
        # Regular instructions.
        if mnemonic == "shl" or mnemonic == "shr":
            # SHL and SHR (no operands).
            if insn.args:
                raise AsmError(f"Too many operands for {mnemonic}", insn.args[1].loc.including(insn.args[-1].loc))
            encode_insn(+cmp_mode, alu_modes[mnemonic], None, insn.loc)
            # Skip one arg check.
            return
            
        elif mnemonic in alu_modes:
            # Other ALU instructions.
            if len(insn.args) == 0:
                raise AsmError(f"Expected value", insn.loc)
            encode_insn(+cmp_mode, alu_modes[mnemonic], insn.args[0], insn.loc)
            
        elif mnemonic in branch_modes:
            # Branches.
            if len(insn.args) == 0:
                raise AsmError(f"Expected memory reference", insn.loc)
            elif insn.args[0].type == OperandType.IMM:
                raise AsmError(f"Expected memory reference", insn.args[0].loc)
            encode_insn(3, branch_modes[mnemonic], insn.args[0], insn.loc)
        
        elif mnemonic == 'li':
            # Load immediate.
            if len(insn.args) == 0:
                raise AsmError(f"Expected immediate value", insn.loc)
            elif insn.args[0].type != OperandType.IMM:
                raise AsmError(f"Expected immediate value", insn.args[0].loc)
            encode_insn(0, 5, insn.args[0], insn.loc)
        
        elif mnemonic == 'st':
            # Memory store.
            if len(insn.args) == 0:
                raise AsmError(f"Expected memory reference", insn.loc)
            elif insn.args[0].type == OperandType.IMM:
                raise AsmError(f"Expected memory reference", insn.args[0].loc)
            encode_insn(2, 0, insn.args[0].loc, insn.loc)
            
        else:
            # Illegal instruction.
            raise AsmError(f"No such instruction `{mnemonic}`", insn.loc)
        
        if len(insn.args) > 1:
            # Instructions other than SHL and SHR have exactly one operand.
            raise AsmError("Too many operands", insn.args[1].loc.including(insn.args[-1].loc))
    
    
    # Pass 1: Write data and reloc entries, find labels.
    line: list[Token] = []
    for token in raw:
        if token.type != TokenType.NEWLINE and token != Token(';', TokenType.OTHER):
            line.append(token)
            continue
        try:
            while len(line) >= 2 and line[0].type == TokenType.IDENTIFIER and line[1] == Token(':', TokenType.OTHER):
                handle_label(line[0].val)
                line = line[2:]
            if len(line) == 0: continue
            if line[0].type != TokenType.IDENTIFIER:
                raise AsmError("Expected instruction or directive", line[0].loc)
            elif line[0].val[0] == '.':
                handle_directive(line[0], line[1:])
            else:
                handle_insn(Insn.parse(line, equ))
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
            case RelocType.BYTES:
                pass
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
