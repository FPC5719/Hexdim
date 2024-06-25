module Hexdim.Section.Arith(Arith) where

import Hexdim.Datatype
import Hexdim.Section

import Clash.Prelude hiding (Xor)
import Control.Lens(scribe, set, (&))
import Data.Default()

data Arith

data ArithInstr
  = Add  | Nand | Xor
  | Flip | Comp | Shr | Send

instance Section Arith where
  type Decoded Arith =
    ( ArithInstr
    , Value  -- Reg 1
    , Value  -- Reg 2
    , RegSel -- Reg Dst
    )
  type Buffer Arith =
    ( RegSel -- Reg Dst
    , Value  -- Result
    , Bool   -- Zero?
    , Bool   -- Overflow?
    )

  decoder rs = \case
    $(bitPattern "11rrccss") -> Just $
      let r1 = bitCoerce rr :: RegSel
          r2 = bitCoerce ss :: RegSel
      in case cc of
        0b00 -> (Add , rs !! r1, rs !! r2, r1)
        0b01 -> (Nand, rs !! r1, rs !! r2, r1)
        0b10 -> (Xor , rs !! r1, rs !! r2, r1)
        _    -> case ss of
          0b00 -> (Flip, rs !! r1, 0, r1)
          0b01 -> (Comp, rs !! r1, 0, r1)
          0b10 -> (Shr , rs !! r1, 0, r1)
          _    -> (Send, rs !! r1, 0, r1)
    _ -> Nothing
  
  onDecode fwd (op, r1, r2, dst) = pure $
    ( case op of
        Add ->
          let ansf = ( (zeroExtend r1 :: Unsigned 9)
                     + (zeroExtend r2 :: Unsigned 9)
                     ) :: Unsigned 9
              ans = truncateB ansf :: Unsigned 8
          in (dst, ans, ans == 0, ansf > 255)
        Nand ->
          let ans = complement (r1 .&. r2)
          in (dst, ans, ans == 0, False)
        Xor ->
          let ans = r1 `xor` r2
          in (dst, ans, ans == 0, False)
        Flip ->
          let hi = r1 `shiftR` 4 :: Unsigned 8
              lo = zeroExtend (truncateB r1 :: Unsigned 4)
                :: Unsigned 8
              ans = (lo `shiftL` 4) + hi
          in (dst, ans, ans == 0, False)
        Comp ->
          let ansf = ( (zeroExtend (complement r1) :: Unsigned 9)
                     + (1 :: Unsigned 9)
                     ) :: Unsigned 9
              ans = truncateB ansf :: Unsigned 8
          in (dst, ans, ans == 0, ansf > 255)
        Shr ->
          let ans = r1 `shiftR` 1
          in (dst, ans, ans == 0, testBit r1 0)
        Send ->
          let ans = r1
          in (dst, ans, ans == 0, False)
    , def )
  
  onExecute (dst, res, zr, ov) = do
    scribe statusW . pure $ (zr, ov)
    pure $ ( Just (dst, res)
           , def & set fStatus (pure (zr, ov))
           )
