module Hexdim.Testbench(testbench) where

import Hexdim.Datatype
import Hexdim.Utility
import Hexdim.Circuit

import Clash.Prelude
import Clash.Prelude.Testbench
import qualified Clash.Sized.Vector as V
import qualified Data.List as L

instrToVec :: [Instr] -> Vec 256 Instr
instrToVec xs = if l > 256
  then error "Too many instructions!"
  else case V.fromList (xs L.++ L.replicate (256 - l) 0) of
         Just v -> v
         Nothing -> error "Unintended behavior"
  where l = L.length xs

testBoard :: forall dom . HiddenClockResetEnable dom
          => [Instr]
          -> Signal dom Value
testBoard xs = result
  where (ia, da, dout, srw, smp, sen) =
          board imem dmem
        imem = rom (instrToVec xs) ia
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

testbench :: [Instr] -> Signal System Value
testbench = exposeClockResetEnable testBoard clk rst en
  where clk = tbSystemClockGen (pure True)
        rst = systemResetGen
        en = tbEnableGen
