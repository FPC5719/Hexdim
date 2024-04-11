module Hexdim.Circ where

import Hexdim.Prim

import Clash.Prelude
import Clash.Annotations.TH

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Wire
  -> Signal System Wire
topEntity = exposeClockResetEnable 
makeTopEntity 'topEntity
