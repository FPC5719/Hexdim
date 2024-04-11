## Introduction

Hexdim (HEXDeci-Instruction Machine) is an 8-bit CPU built in Haskell-Clash.

Features (Not implemented yet):

- Simple enough to implement, while complex enough to perform several tasks.
- 5-stage pipeline, easily composable in monadic style.
- Extensive use of `Lens`.
- Code reuse with different compiler backends: GHC and Clash.
- Easy DiffTest.

## Build steps

To build the project, run:

```
cabal build
```

To open REPL, run:

```
cabal run clashi
```

To export Verilog, run:

```
cabal run clash -- Hexdim.Circ --verilog
```

## Design details

Below is the table of all 16 instructions:

| Op   | Description                         | 7-6 | 5-4 | 3-2 | 1-0 |
|------|-------------------------------------|-----|-----|-----|-----|
| NOP  | Do Nothing                          | 00  | --  | 00  | --  |
| JMP  | Jump                                | 00  | R1  | 01  | --  |
| JZ   | Jump if Zero                        | 00  | R1  | 10  | --  |
| JO   | Jump if Overflow                    | 00  | R1  | 11  | --  |
| STR  | Store register into memory          | 01  | R1  | 00  | R2  |
| LOAD | Load register from memory           | 01  | R1  | 01  | R2  |
| OUT  | Output register                     | 01  | R1  | 10  | R2  |
| IN   | Input register                      | 01  | R1  | 11  | R2  |
| SETL | Set lower bits                      | 10  | R1  | IM  | IM  |
| ADD  | R1 = R1 + R2 (Overflow)             | 11  | R1  | 00  | R2  |
| NAND | R1 = ~(R1 & R2)                     | 11  | R1  | 01  | R2  |
| XOR  | R1 = R1 ^ R2                        | 11  | R1  | 10  | R2  |
| FLIP | R1[7-4], R1[3-0] = R1[3-0], R1[7-4] | 11  | R1  | 11  | 00  |
| COMP | R1 = ~R1 + 1 (Overflow)             | 11  | R1  | 11  | 01  |
| SHR  | R1 = R1 >> 1 (Overflow)             | 11  | R1  | 11  | 10  |
| SEND | R1 = R1                             | 11  | R1  | 11  | 11  |

