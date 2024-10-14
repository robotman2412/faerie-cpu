
# Faerie 8-bit CPU
Size-optimized. WIP. The README is also WIP.

Copyright © 2024, Julian Scheffers. Licenced under [CC BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/).

# Instruction Set & Architecture
Note: Zero-page pointers need a 2-byte alignment. The LSB of the 0-page pointer address is currently reserved.

## Instruction format

Visual representation:
```
7... ...0
CMMM AAOO
```

| Symbol | Bit range | Description
| :----: | :-------: | :----------
|   C    |     7     | Carry mode for the ALU; 0: default, 1: from carry flag
|   M    |    6-4    | Mode bits for the ALU / branch condition
|   A    |    3-2    | Addressing mode
|   O    |    1-0    | Opcode

## Opcode summary
| Addr mode | Opcode | Operand           | Used for   | Description
| :-------: | :----: | :---------------- | :--------- | :----------
|     0     |    0   | Zero-page address | Arith,load | Perform ALU operation with accumulator and value from zero-page memory
|     1     |    0   | Zero-page pointer | Arith,load | Perform ALU operation with accumulator and value from zero-page pointer
|     2     |    0   | Address           | Arith,load | Perform ALU operation with accumulator and value from memory
|     3     |    0   | Immediate         | Arith,load | Perform ALU operation with A and immediate value
|     3     |    1   | -                 | SHL,load 0 | Perform ALU operation with A and constant 0
|     0     |    2   | Zero-page address | Store      | Store accumulator to zero-page memory
|     1     |    2   | Zero-page pointer | Store      | Store accumulator to pointer from zero-page
|     2     |    2   | Address           | Store      | Store accumulator to memory
|     1     |    3   | Zero-page pointer | Branch     | Jump / branch to zero-page pointer address
|     2     |    3   | Address           | Branch     | Jump / branch to bit address

## Arithmetic and logic unit
The ALU is designed to be as compact as possible while still supporting the ADD/SUB/AND/OR/XOR/SHL/SHR expected by most modern programming languages. It also has a pass-through mode for the B operand for things like loading from memory or loading an immediate.

### Mode bit names
| S2      | S1      | S0
| :------ | :------ | :------
| LOGIC/B | SHL/XOR | SUB/AND

### ALU modes
| S2  | S1  | S0  | Arithmetic operation
| :-: | :-: | :-: | :-------------------
|  0  |  0  |  0  | A + (A ^ B)
|  0  |  0  |  1  | A ^ B
|  0  |  1  |  0  | A + B
|  0  |  1  |  1  | A \| B
|  1  |  0  |  0  | A >> 1
|  1  |  0  |  1  | B
|  1  |  1  |  0  | A - B
|  1  |  1  |  1  | A & B
