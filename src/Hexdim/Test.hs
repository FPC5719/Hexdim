module Hexdim.Test where

import Hexdim.Datatype
import Hexdim.Circuit

import Clash.Prelude
import Clash.Annotations.TH

romPath :: String
romPath = "test.rom"

testBoard :: HiddenClockResetEnable dom
          => Signal dom Value
testBoard = regEn 0 ren dout
  where (ia, da, dout, srw, smp, sen) =
          board imem dmem
        imem = bitCoerce <$> (romFilePow2 romPath ia)
        dmem = blockRamPow2 (replicate SNat 0) da $
          pure (\x y z a d -> case (x, y, z) of
                   (True, False, True) -> Just (a, d)
                   _ -> Nothing
               )
          <*> (bitToBool <$> srw)
          <*> (bitToBool <$> smp)
          <*> (bitToBool <$> sen)
          <*> da
          <*> dout
        ren = pure (\x y z -> case (x, y, z) of
                       (True, True, True) -> True
                       _ -> False
                   )
              <*> (bitToBool <$> srw)
              <*> (bitToBool <$> smp)
              <*> (bitToBool <$> sen)

testTop
  :: "CLK" ::: Clock System
  -> "RST" ::: Reset System
  -> "EN"  ::: Enable System
  -> "OUT" ::: Signal System Value
testTop = exposeClockResetEnable testBoard
makeTopEntity 'testTop
