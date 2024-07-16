module Hexdim.Section.Immed(Immed) where

import Hexdim.Datatype
import Hexdim.Section

import Clash.Prelude

data Immed

instance Section Immed where
  type Decoded Immed = (RegSel, Value, Imm)
  type Buffer Immed = (RegSel, Value)

  decoder rs = \case
    $(bitPattern "10rrmmmm") ->
      let r = bitCoerce rr :: RegSel
      in Just (r, rs !! r, bitCoerce mmmm)
    _ -> Nothing
      
  onDecode fwd (dst, val, imm) = pure $
    ((dst, val .&. 0b11110000 .|. (zeroExtend imm)), def)

  onExecute (dst, val) = pure $
    (Just (dst, val), def)
