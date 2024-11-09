
# Faerie 8-bit CPU
Size-optimized. WIP. The README is also WIP.

Copyright © 2024, Julian Scheffers. Licenced under [CC BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/).



# Memory Map
| Start  | End    | Description
| :----- | :----- | :----------
| `0000` | `7fff` | RAM
| `8000` | `801f` | LED matrix display (write-only)
| `8020` | `8fff` | Mirror of LED matrix display
| `9000` | `9003` | GPIO pins
| `8020` | `bfff` | Empty
| `c000` | `cfff` | ROM

## Region: RAM
There is one contiguous region of RAM (Random-Access Memory) from `0x0000` to `0x7fff`. The Faerie CPU splits RAM into "zero-page" RAM and the remaining, normal RAM.
Zero-page RAM is a CPU concept; Faerie can access zero-page RAM faster than normal RAM and pointers can only be dereferenced from zero-page RAM.
For more information, see [Instruction Set & Architecture](#instruction-set--architecture).

## Region: GPIO pins
The GPIO pins region allows the CPU to read the buttons and interact with one or more of the 8 GPIO (General-Purpose Input/Output) pins. Each GPIO pin can be individually set to input, open-drain (floating or low), open-source (high or floating) or output using the direction registers.

| Reg | Name | Description
| :-: | :--- | :----------
|  0  | PIN  | Read input-only pins (the buttons)
|  1  | PIO  | Read/write GPIOs
|  2  | PHE  | Enable high level
|  3  | PLE  | Enable low level

To set a pin to one of the following modes, set its corresponding PLE and PHE bits accordingly:
| PLE | PHE | Description
| :-: | :-: | :----------
|  0  |  0  | Input pin
|  0  |  1  | Open-source (high or floating) pin
|  1  |  0  | Open-drain (floating or low) pin
|  1  |  1  | Output pin



# Instruction Set & Architecture
The Faerie CPU is an 8-bit von Neumann CPU with a 16-bit address bus and a single general-purpose register.
All calculations and data movement operations go through this register (the accumulator).
To make Faerie not completely useless in the face of having only one register, it has multiple memory addressing modes that make pointers and a soft-stack possible.

Unlike most modern even most 1970s CPUs, Faerie supports neither a stack nor interrupts in hardware; the stack can be emulated in software but interrupts do not exist.
This is a compromise used to reduce the size of Faerie; most code will not use the stack so often that it becomes detrimental to use a soft-stack.
Unfortunately, not supporting interrupts means that asynchronicity is not a thing; anything that is otherwise an interrupt (like button presses, GPIO level changes and timers) must be handled entirely in software.

## Addressing mode summary
To keep Faerie flexible enough to be useful, it has three memory addressing modes and one immediate addressing mode:
| Addr mode | Operand size  | Description
| :-------: | :-----------: | :----------
|     0     |       1       | Zero-page address; access a byte between address `0x00` and `0xff`
|     1     |       1       | Zero-page pointer; use two bytes in the zero page as a pointer to the byte to access, which may be anywhere in memory
|     2     |       2       | Any address; access a byte anywhere in memory
|     3     |       1       | Immediate value; use an immediate value for an ALU operation.

**WARNING: Zero-page pointers need a 2-byte alignment. If misaligned, the pointer will not be dereferenced correctly.**

All instructions use either a memory operand or an immediate operand (only legal for ALU opcodes)
There is only one exception to this: Unlike all other instructions, the SHL and SHR do not load anything and do not have an address/operand byte and they must have the addressing mode set to 3 (as opposed to multiple valid addressing modes for all other instructions).



## Instruction format
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
|     2     |    3    | Address           | Branch     | Jump / branch to fixed address



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
