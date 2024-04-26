module Hexdim.Pipe.Arith
  ( arith, arithW, ArithInstr
  ) where

import Clash.Prelude
import Control.Lens
import Data.Bits()

import Hexdim.Pipe.Data

data ArithInstr
  = ArNop
  | ArAdd  Reg Wire Wire
  | ArNand Reg Wire Wire
  | ArXor  Reg Wire Wire
  | ArFlip Reg Wire
  | ArComp Reg Wire
  | ArShr  Reg Wire
  | ArSend Reg Wire

arith :: Monad m => Wire -> Pipe () m ArithInstr
arith = \case
  $(bitPattern "11rrllss") -> do
    scribe regS1 $ pure rr
    scribe regS2 $ pure ss
    let biop f = f rr <$> view regR1 <*> view regR2
        unop f = f rr <$> view regR1
    case ll of
      0b00 -> biop ArAdd
      0b01 -> biop ArNand
      0b10 -> biop ArXor
      0b11 -> case ss of
        0b00 -> unop ArFlip
        0b01 -> unop ArComp
        0b10 -> unop ArShr
        0b11 -> unop ArSend
        _ -> return ArNop
      _ -> return ArNop
  _ -> return ArNop

arithW :: Monad m => ArithInstr -> Pipe () m ()
arithW = \case
  ArNop -> return ()
  ArAdd d r1 r2 -> do
    let ans = zeroExtend r1 + zeroExtend r2 :: Unsigned 9
    scribe statusW . pure . Just $
      ( popCount (clearBit ans 9) == 0
      , testBit ans 9 )
    scribe regSW . pure . Just $ d
    scribe regW . pure . Just $ truncateB ans
  ArNand d r1 r2 ->
    common d $ complement (r1 .&. r2)
  ArXor d r1 r2 ->
    common d $ r1 `xor` r2
  ArFlip d r -> case r of
    $(bitPattern "hhhhllll") ->
      common d $ bitCoerce (llll ++# hhhh)
    _ -> return ()
  ArComp d r -> do
    let ans = zeroExtend (complement r) + 1 :: Unsigned 9
    scribe statusW . pure . Just $
      ( popCount (clearBit ans 9) == 0
      , testBit ans 9 )
    scribe regSW . pure . Just $ d
    scribe regW . pure . Just $ truncateB ans
  ArShr d r -> do
    let ans = shiftR r 1
    scribe statusW . pure . Just $
      ( popCount ans == 0
      , testBit r 0 )
    scribe regSW . pure . Just $ d
    scribe regW . pure . Just $ ans
  ArSend d r ->
    common d r

common :: Monad m => Reg -> Wire -> Pipe () m ()
common d ans = do
  scribe statusW . pure . Just $
    ( popCount ans == 0, False )
  scribe regSW . pure . Just $ d
  scribe regW . pure . Just $ ans
