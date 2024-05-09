module Hexdim.Dsl where

import Clash.Prelude
import Data.Array
import qualified Data.List as L

import Hexdim.Pipe.Data

_r0, _r1, _r2, _r3 :: Reg
_r0 = 0
_r1 = 1
_r2 = 2
_r3 = 3

_nop :: Wire
_nop = merge4 0 0 0 0

_jmp, _jz, _jo :: Reg -> Wire
_jmp  r1 = merge4 0 r1 1 0
_jz   r1 = merge4 0 r1 2 0
_jo   r1 = merge4 0 r1 3 0

_str, _load, _out, _in :: Reg -> Reg -> Wire
_str  r1 r2 = merge4 1 r1 0 r2
_load r1 r2 = merge4 1 r1 1 r2
_out  r1 r2 = merge4 1 r1 2 r2
_in   r1 r2 = merge4 1 r1 3 r2

_setl :: Reg -> Imm -> Wire
_setl r1 im = merge3 2 r1 im

_add, _nand, _xor :: Reg -> Reg -> Wire
_add  r1 r2 = merge4 3 r1 0 r2
_nand r1 r2 = merge4 3 r1 1 r2
_xor  r1 r2 = merge4 3 r1 2 r2

_flip, _comp, _shr, _send :: Reg -> Wire
_flip r1 = merge4 3 r1 3 0
_comp r1 = merge4 3 r1 3 1
_shr  r1 = merge4 3 r1 3 2
_send r1 = merge4 3 r1 3 3

merge4 :: Reg -> Reg -> Reg -> Reg -> Wire
merge4 a b c d = bitCoerce $
  bitCoerce a ++# bitCoerce b ++#
  bitCoerce c ++# bitCoerce d

merge3 :: Reg -> Reg -> Imm -> Wire
merge3 a b c = bitCoerce $
  bitCoerce a ++# bitCoerce b ++#
  bitCoerce c

dumpInstr :: [Wire] -> Array Addr Wire
dumpInstr xs = if l > 256
  then error "Too many instructions!"
  else listArray (0, 255) (xs L.++ L.replicate (256 - l) 0)
  where l = L.length xs
