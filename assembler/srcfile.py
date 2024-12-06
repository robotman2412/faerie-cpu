
from typing_extensions import Self

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
        self.macro_inst: Location = None
    
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
