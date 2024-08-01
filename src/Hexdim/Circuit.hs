module Hexdim.Circuit where

import Hexdim.Datatype
import Hexdim.Core

import Clash.Prelude
import Clash.Annotations.TH
import Control.Lens(view)

board :: HiddenClockResetEnable dom
      =>   Signal dom Instr
      ->   Signal dom Value
      -> ( Signal dom Addr
         , Signal dom Addr
         , Signal dom Value
         , Signal dom Bit
         , Signal dom Bit
         , Signal dom Bit
         )
board op val = unwrap $ mealy core def cr
  where cr = pure CoreR <*> op <*> val
        unwrap cw = ( view coreInstrA <$> cw
                    , view coreDataA  <$> cw
                    , view coreDataW  <$> cw
                    , bitCoerce . view coreSelRW <$> cw
                    , bitCoerce . view coreSelMP <$> cw
                    , bitCoerce . view coreSelEn <$> cw
                    )


topEntity
  ::   "CLK"    ::: Clock System
  ->   "RST"    ::: Reset System
  ->   "EN"     ::: Enable System
  ->   "Instr"  ::: Signal System Instr
  ->   "DataI"  ::: Signal System Value
  -> ( "IAddr"  ::: Signal System Addr
     , "DAddr"  ::: Signal System Addr
     , "DataO"  ::: Signal System Value
     , "SelRW"  ::: Signal System Bit
     , "SelMP"  ::: Signal System Bit
     , "SelEn"  ::: Signal System Bit
     )
topEntity = exposeClockResetEnable board
makeTopEntity 'topEntity
