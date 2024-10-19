
# Faerie 8-bit CPU
Size-optimized. WIP. The README is also WIP.

Copyright © 2024, Julian Scheffers. Licenced under [CC BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/).

# Instruction Set & Architecture
Note: Zero-page pointers need a 2-byte alignment. The LSB of the 0-page pointer address is currently reserved and must be 0.

## Instruction format

Visual representation:
```
7... ...0
MMMM AAOO
```

| Symbol | Bit range | Description
| :----: | :-------: | :----------
|   M    |    7-4    | Mode bits for the ALU / branch condition
|   A    |    3-2    | Addressing mode
|   O    |    1-0    | Opcode

## Opcode summary
| Addr mode | Opcode  | Operand           | Used for   | Description
| :-------: | :-----: | :---------------- | :--------- | :----------
|     0     |   0/1   | Zero-page address | Arith,load | Perform ALU operation with value from zero-page memory
|     1     |   0/1   | Zero-page pointer | Arith,load | Perform ALU operation with value from zero-page pointer
|     2     |   0/1   | Address           | Arith,load | Perform ALU operation with value from memory
|     3     |   0/1   | Immediate         | Arith,load | Perform ALU operation with immediate value
|     0     |    2    | Zero-page address | Store      | Store to zero-page memory
|     1     |    2    | Zero-page pointer | Store      | Store to pointer from zero-page
|     2     |    2    | Address           | Store      | Store to memory
|     1     |    3    | Zero-page pointer | Branch     | Jump / branch to zero-page pointer address
|     2     |    3    | Address           | Branch     | Jump / branch to bit address

## Branch instructions
If the M3 bit is set, the branch is unconditional and M2-M0 should be 0. If the M3 bit is clear, 1 of 8 branch conditions may be selected with M2-M0.

### Branch conditions
| M2  | M1  | M0  | Flag condition     | Comparison
| :-: | :-: | :-: | :----------------- | :---------
|  0  |  0  |  0  | zero               | a == b
|  0  |  0  |  1  | carry              | (a + b) >= 256
|  0  |  1  |  0  | carry && !zero     | a > b
|  0  |  1  |  1  | !carry && !zero    | a < b
|  1  |  0  |  0  | !(zero)            | a != b
|  1  |  0  |  1  | !(carry)           | (a + b) < 256
|  1  |  1  |  0  | !(carry && !zero)  | a <= b
|  1  |  1  |  1  | !(!carry && !zero) | a >= b

## Arithmetic and logic unit
The ALU is designed to be as compact as possible while still supporting the ADD/SUB/AND/OR/XOR/SHL/SHR expected by most modern programming languages. It also has a pass-through mode for the B operand for things like loading from memory or loading an immediate.

### ALU modes
Flag behaviours:
| M3  | Opcode  | Carry in  | Use flags
| :-: | :-----: | :-------: | :-------:
|  0  |    0    |  default  |     y
|  1  |    0    | from flag |     y
|  0  |    1    |  default  |     n
|  1  |    1    | from flag |     n

Arithmetic and logic operations:
| M2  | M1  | M0  | Has arg | Arithmetic operation
| :-: | :-: | :-: | :-----: | :-------------------
|  0  |  0  |  0  |    n    | A << 1
|  0  |  0  |  1  |    y    | A ^ B
|  0  |  1  |  0  |    y    | A + B
|  0  |  1  |  1  |    y    | A \| B
|  1  |  0  |  0  |    n    | A >> 1
|  1  |  0  |  1  |    y    | B
|  1  |  1  |  0  |    y    | A - B
|  1  |  1  |  1  |    y    | A & B
