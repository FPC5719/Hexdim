module Hexdim.Instruction where

import Hexdim.Datatype

import Clash.Prelude

_r0, _r1, _r2, _r3 :: RegSel
_r0 = 0
_r1 = 1
_r2 = 2
_r3 = 3

_nop :: Instr
_nop = merge4 0 0 0 0

_jmp, _jz, _jo :: RegSel -> Instr
_jmp  r1 = merge4 0 r1 1 0
_jz   r1 = merge4 0 r1 2 0
_jo   r1 = merge4 0 r1 3 0

_str, _load, _out, _in :: RegSel -> RegSel -> Instr
_str  r1 r2 = merge4 1 r1 0 r2
_load r1 r2 = merge4 1 r1 1 r2
_out  r1 r2 = merge4 1 r1 2 r2
_in   r1 r2 = merge4 1 r1 3 r2

_setl :: RegSel -> Immed -> Instr
_setl r1 im = merge3 2 r1 im

_add, _nand, _xor :: RegSel -> RegSel -> Instr
_add  r1 r2 = merge4 3 r1 0 r2
_nand r1 r2 = merge4 3 r1 1 r2
_xor  r1 r2 = merge4 3 r1 2 r2

_flip, _comp, _shr, _send :: RegSel -> Instr
_flip r1 = merge4 3 r1 3 0
_comp r1 = merge4 3 r1 3 1
_shr  r1 = merge4 3 r1 3 2
_send r1 = merge4 3 r1 3 3

merge4 :: RegSel -> RegSel -> RegSel -> RegSel -> Instr
merge4 a b c d = bitCoerce $
  bitCoerce a ++# bitCoerce b ++#
  bitCoerce c ++# bitCoerce d

merge3 :: RegSel -> RegSel -> Immed -> Instr
merge3 a b c = bitCoerce $
  bitCoerce a ++# bitCoerce b ++#
  bitCoerce c
