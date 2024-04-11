module Hexdim.Prim where

import Clash.Prelude

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Unsigned 8)
  -> Signal System (Unsigned 8)
topEntity = exposeClockResetEnable (fmap id)
