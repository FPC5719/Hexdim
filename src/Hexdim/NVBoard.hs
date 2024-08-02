module Hexdim.NVBoard where

import Hexdim.Test

import Clash.Prelude
import Clash.Annotations.TH

toSeg8 :: BitVector 8 -> (Unsigned 8, Unsigned 8)
toSeg8 = \case
  $(bitPattern "hhhhllll") ->
    ( segData !! (bitCoerce hhhh :: Unsigned 4)
    , segData !! (bitCoerce llll :: Unsigned 4)
    )
  _ -> (0, 0)

segData :: Vec 16 (Unsigned 8)
segData = map complement
  (  0b11111100 :> 0b01100000 :> 0b11011010 :> 0b11110010
  :> 0b01100110 :> 0b10110110 :> 0b10111110 :> 0b11100000
  :> 0b11111110 :> 0b11110110 :> 0b11101110 :> 0b00111110
  :> 0b10011100 :> 0b01111010 :> 0b10011110 :> 0b10001110
  :> Nil )

nvBoard :: HiddenClockResetEnable dom
        => ( Signal dom (Unsigned 8)
           , Signal dom (Unsigned 8)
           )
nvBoard = (snd <$> seg, fst <$> seg)
  where seg = toSeg8 . bitCoerce <$> testBoard

nvTop
  ::   "CLK"    ::: Clock System
  ->   "RST"    ::: Reset System
  ->   "EN"     ::: Enable System
  -> ( "Seg0"   ::: Signal System (Unsigned 8)
     , "Seg1"   ::: Signal System (Unsigned 8)
     )
nvTop = exposeClockResetEnable nvBoard
makeTopEntity 'nvTop

