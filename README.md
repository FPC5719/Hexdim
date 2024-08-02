## Introduction

Hexdim (HEXaDeci-Instruction Machine) is an 8-bit CPU built in Haskell-Clash.

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

## Programming in Hexdim

Hexdim Assembly uses an eDSL described in `Hexdim.Instruction`.

For example, the following program describes a counter, which
increases the output by 1 every 4 cycles.

``` haskell
[ _setl _r3 1
, _out  _r1 _r0
, _nand _r1 _r1
, _comp _r1
, _jmp  _r3
]
```

And here is a more complicated example: calculating primes. It uses
every single instruction. (except `In`, which is unimplemented for now)

``` haskell
[ _setl _r0 2   -- - i = 2
, _xor  _r3 _r3 -- \                   {loop} = 1
, _str  _r0 _r3 -- / [0] = i
, _xor  _r1 _r1 -- \
, _setl _r1 1   -- / j = 1
, _xor  _r3 _r3 -- \                   {while} = 5
, _load _r0 _r3 -- / i = [0]
, _setl _r3 15  -- \
, _add  _r3 _r3 -- / a = 30{end}
, _load _r2 _r1 -- \ k = [j]
, _send _r2     -- | if k == 0
, _jz   _r3     -- / go {end}
, _xor  _r3 _r3 -- \
, _setl _r3 15  -- / a = 15{mod}
, _comp _r2     -- - k = -[j]
, _add  _r0 _r2 -- \ i = i - [j]       {mod} = 15
, _jo   _r3     -- / if i >= 0 go {mod}
, _comp _r2     -- \
, _add  _r0 _r2 -- / i = i + [j]
, _xor  _r3 _r3 -- \
, _setl _r3 15  -- | a = 30{end}
, _add  _r3 _r3 -- /
, _send _r0     -- \ if i == 0
, _jz   _r3     -- / go {end}
, _nand _r1 _r1 -- \
, _comp _r1     -- / j = j + 1
, _xor  _r3 _r3 -- \
, _setl _r3 5   -- / a = 5{while}
, _jmp  _r3     -- - go {while}
, _nop          -- - padding
, _xor  _r3 _r3 -- \                   {end} = 30
, _load _r0 _r3 -- / i = [0]
, _setl _r3 2   -- \
, _flip _r3     -- | a = 42{yes}
, _setl _r3 10  -- /
, _load _r2 _r1 -- - k = [j]
, _send _r2     -- \ if [j] == 0
, _jz   _r3     -- / go {yes}
, _setl _r3 2   -- \
, _flip _r3     -- | a = 45{final}
, _setl _r3 13  -- /
, _jmp  _r3     -- - go {final}
, _str  _r0 _r1 -- - [j] = i           {yes} = 42
, _xor  _r3 _r3 -- \
, _out  _r0 _r3 -- / out[0] = i
, _nand _r0 _r0 -- \                   {final} = 45
, _comp _r0     -- / i = i + 1
, _xor  _r3 _r3 -- \
, _setl _r3 1   -- / a = 1{loop}
, _jmp  _r3     -- - go {loop}
]
```

For hardware synthesis, the program should be converted into `ROMFILE`.

In REPL, run:

``` Haskell
import Hexdim.ROM
import Hexdim.Instruction
:{
[ -- Program here
]
:}
writeROM it
```

And `ROMFILE` would be created under the project directory.

## Debugging and Running

The project can run in 3 different levels:

### Running pure Haskell code in Clashi

Let `instr` be the program. In REPL, run:

```
import Hexdim.Emulator
withInstr instr
emulate it
```

Step with `emulate it` and the result would be printed.

### Running testbench in Clashi

First export the program to `ROMFILE`. In REPL, type:

```
import Hexdim.Test
import Clash.Prelude
import Clash.Prelude.Testbench
sampleN 30 $ testTop (tbSystemClockGen (pure True)) systemResetGen tbEnableGen
```

Which would print the result of the first 30 cycles.

### Running Verilog with Verilator

First export the program to `ROMFILE`, and then:

```
cabal run clash -- Hexdim.Test --verilog
cp ROMFILE ./verilator/ROMFILE
cd ./verilator
make test
```

---

In addition, the project can be integrated with NJU VBoard.

First export the program to `ROMFILE`, and then:

```
cabal run clash -- Hexdim.NVBoard --verilog
cp ROMFILE ./verilator/ROMFILE
cd ./verilator
make nvboard
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
indicated by bit[7-6].

- `00`: Branch
- `01`: Memory
- `10`: Immediate value
- `11`: Arithmetic

### Registers

`Zero` is set/reset according to whether the result of an Arithmetic instruction equals 0,
and is left unchanged after any other instructions.

`Overflow` is set/reset according to whether the result of an Arithmetic instruction overflows,
and is left unchanged after any other instructions. Currently, only `ADD`, `COMP` and `SHR` may
cause overflow.

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