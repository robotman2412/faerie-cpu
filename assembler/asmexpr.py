
from enum import Enum

from srcfile import *
from asmerr import *
from tokenizer import *

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

def parse_expr(args: list[Token|SymRef], equ: dict[str,int] = {}, start_loc: Location = None) -> SymRef:
    # Could I do this with an LR parser? Yes.
    # Will I convert this into an LR parser? No.
    if len(args) == 0:
        raise AsmError("Expected expression", start_loc)
    
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
        args = args[:i] + [parse_expr(args[i+1:x-1], equ, args[i+1].loc)] + args[x:]
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
