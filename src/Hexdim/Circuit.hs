module Hexdim.Circuit(topEntity, board, pipe) where

import Hexdim.Datatype
import Hexdim.Utility
import Hexdim.Pipeline

import Clash.Prelude
import Clash.Annotations.TH
import Control.Lens(view, _1, _2, _3, _4, _5)
import Control.Monad.RWS
import Data.Maybe
import Data.Monoid()

-- type Debug = ()

pipe :: (HiddenClockResetEnable dom)
     => Signal dom PipeR
     -> Signal dom PipeW
pipe = flip mealy def $ \bufd r ->
  let (bufd', (), w) = runRWS (pipeM bufd) r ()
  in (bufd', w)

regCycle0 :: HiddenClockResetEnable dom
          => Signal dom Bool
regCycle0 = register False (pure True)

regPC :: HiddenClockResetEnable dom
      => Signal dom PipeW
      -> Signal dom Addr
regPC w = regMaybe 0 (viewFirst w counterW)

regStatus :: HiddenClockResetEnable dom
          => Signal dom PipeW
          -> Signal dom (Bool, Bool)
regStatus w = regMaybe (False, False) (viewFirst w statusW)

regGeneral :: HiddenClockResetEnable dom
           => Signal dom PipeW
           -> Signal dom RegBank
regGeneral w = inner
  where inner = register (0 :> 0 :> 0 :> 0 :> Nil) rnew
        rnew = pure replaceMaybe
               <*> ( pushTuple <$> pushTuple
                     ( viewFirst w regDst
                     , viewFirst w regW
                     )
                   )
               <*> inner

board :: HiddenClockResetEnable dom
      =>   Signal dom Instr
      ->   Signal dom Value
      -> ( Signal dom Addr
         , Signal dom Addr
         , Signal dom Value
         , Signal dom Bit
         , Signal dom Bit
         , Signal dom Bit
         -- , Signal dom Debug -- Debug
         )
board op val =
  let r = pure PipeR
        <*> regCycle0
        <*> regPC w
        <*> op
        <*> regGeneral w
        <*> regStatus w
        <*> val
        <*> val
      w = pipe r
      resultSel =
        pure (\ma mw pa pw -> case (ma, mw) of
                 (Nothing, _) -> case (pa, pw) of
                   (Nothing, _) ->
                     (0, 0, False, False, False)
                   (Just addr, Nothing) ->
                     (addr, 0, False, True, True)
                   (Just addr, Just d) ->
                     (addr, d, True, True, True)
                 (Just addr, Nothing) ->
                   (addr, 0, False, False, True)
                 (Just addr, Just d) ->
                   (addr, d, True, False, True)
             )
        <*> viewFirst w memoryA
        <*> viewFirst w memoryW
        <*> viewFirst w periA
        <*> viewFirst w periW
  in ( bitCoerce . fromMaybe 0 <$> viewFirst w instrA
     , bitCoerce . (view _1) <$> resultSel
     , bitCoerce . (view _2) <$> resultSel
     , bitCoerce . (view _3) <$> resultSel
     , bitCoerce . (view _4) <$> resultSel
     , bitCoerce . (view _5) <$> resultSel
     -- , w
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
     -- , "Debug"  ::: Signal System Debug
     )
topEntity = exposeClockResetEnable board
makeTopEntity 'topEntity
