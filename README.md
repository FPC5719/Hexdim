## Introduction

Hexdim (HEXDeci-Instruction Machine) is an 8-bit CPU built in Haskell-Clash.

The project mainly focuses on the Design Strategies and Infrastructures,
instead of the CPU core itself.

Features:

- Simple enough to implement, while complex enough to perform several tasks.
- Staged pipeline, easily composable in monadic style.
- Hierarchical code structure, separated by functions instead of stages.
- Code reuse with different compiler backends: GHC and Clash.
- Easy DiffTest.

## Build steps

Build the project:

```
cabal build
```

Start REPL:

```
cabal run clashi
```

Export Verilog and run Verilator:

```
cabal run clash -- Hexdim.Test --verilog
verilator --cc --exe --build -j ./verilog/Hexdim.Test.testTop/testTop.v ./verilator/test.cpp -Wno-WIDTHTRUNC
./obj_dir/VtestTop
```

## Design details

### Instructions

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

The instructions fall into 4 categories (or `Section`s in the source code),
indecated by bit[7-6].

- `00`: Branch
- `01`: Memory
- `10`: Immediate value
- `11`: Arithmetic

### Pipeline structure

Hexdim is a staged CPU with `fetch`, `decode` and `execute`.

The behavior of each instruction in every section is implemented under
the typeclass `Section`.

- `fetch`: A universal implementation for all sections.
- `decode`: Each section should implement `decoder` and `onDecode`.
In this stage, register access is provided.
- `execute`: Each section should implement `onExecute`, which has a result
for writing back to registers.

The pipeline is assembled in `Pipeline` where the three stages are defined, 
and automatically handles all data forwards.
The result is a monadic pipeline `pipeM` that has an underlying RWST monad.