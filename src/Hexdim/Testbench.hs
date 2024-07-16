module Hexdim.Testbench(testbench) where

import Hexdim.Datatype
import Hexdim.Circuit

import Clash.Prelude
import Clash.Prelude.Testbench

testBoard :: HiddenClockResetEnable dom
          => Vec 256 Instr
          -> Signal dom Value
testBoard xs = result
  where (ia, da, dout, srw, smp, sen) =
          board imem dmem
        imem = rom xs ia
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
        result = pure (\x y z d -> case (x, y, z) of
                          (True, True, True) -> d
                          _ -> 0
                      )
                 <*> (bitToBool <$> srw)
                 <*> (bitToBool <$> smp)
                 <*> (bitToBool <$> sen)
                 <*> dout

testbench :: Vec 256 Instr -> Signal System Value
testbench = exposeClockResetEnable testBoard clk rst en
  where clk = tbSystemClockGen (pure True)
        rst = systemResetGen
        en = tbEnableGen
