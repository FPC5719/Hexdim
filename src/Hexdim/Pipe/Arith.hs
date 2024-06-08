module Hexdim.Pipe.Arith
  ( arith, arithW, ArithInstr
  ) where

import Clash.Prelude
import Control.Lens
import Data.Bits ()

import Hexdim.Pipe.Data

data ArithInstr
  = ArNop
  | ArAdd  Reg
  | ArNand Reg
  | ArXor  Reg
  | ArFlip Reg
  | ArComp Reg
  | ArShr  Reg
  | ArSend Reg
  deriving (Show, Eq)

instance Default ArithInstr where
  def = ArNop

arith :: Monad m => Wire -> Pipe StageID () m ArithInstr
arith = \case
  $(bitPattern "11rrllss") -> do
    let rr' = bitCoerce rr :: Reg
        ss' = bitCoerce ss :: Reg
    scribe regS1 $ pure rr'
    scribe regS2 $ pure ss'
    case ll of
      0b00 -> return $ ArAdd rr'
      0b01 -> return $ ArNand rr'
      0b10 -> return $ ArXor rr'
      0b11 -> case ss of
        0b00 -> return $ ArFlip rr'
        0b01 -> return $ ArComp rr'
        0b10 -> return $ ArShr rr'
        0b11 -> return $ ArSend rr'
        _ -> return ArNop
      _ -> return ArNop
  _ -> return ArNop

arithW :: Monad m => ArithInstr -> Pipe StageEX () m ()
arithW = \case
  ArNop -> return ()
  ArAdd d -> biop $ \(r1, r2) -> do
    let ans = zeroExtend r1 + zeroExtend r2 :: Unsigned 9
    scribe statusW . pure $
      ( popCount (clearBit ans 9) == 0
      , testBit ans 9 )
    scribeR (d, truncateB ans)
  ArNand d -> biop $ \(r1, r2) ->
    common d $ complement (r1 .&. r2)
  ArXor d -> biop $ \(r1, r2) ->
    common d $ r1 `xor` r2
  ArFlip d -> unop $ \r -> case r of
    $(bitPattern "hhhhllll") ->
      common d $ bitCoerce (llll ++# hhhh)
    _ -> return ()
  ArComp d -> unop $ \r -> do
    let ans = zeroExtend (complement r) + 1 :: Unsigned 9
    scribe statusW . pure $
      ( popCount (clearBit ans 9) == 0
      , testBit ans 9 )
    scribeR (d, truncateB ans)
  ArShr d -> unop $ \r -> do
    let ans = shiftR r 1
    scribe statusW . pure $
      ( popCount ans == 0
      , testBit r 0 )
    scribeR (d, ans)
  ArSend d -> unop $ \r ->
    common d r

common :: Monad m => Reg -> Wire -> Pipe StageEX () m ()
common d ans = do
  scribe statusW . pure $
    ( popCount ans == 0, False )
  scribeR (d, ans)

scribeR :: Monad m => (Reg, Wire) -> Pipe StageEX () m ()
scribeR (d, ans) = scribe regW . pure $ (d, ans)

unop :: Monad m => (Wire -> Pipe StageEX () m ()) -> Pipe StageEX () m ()
unop f = view regR1 >>= f

biop :: Monad m => ((Wire, Wire) -> Pipe StageEX () m ()) -> Pipe StageEX () m ()
biop f = ((,) <$> view regR1 <*> view regR2) >>= f
