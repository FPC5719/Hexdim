module Hexdim.Emulator where

import Hexdim.Datatype
import Hexdim.Pipeline

import Clash.Prelude
import Control.Lens(makeLenses, (^.))
import Control.Monad.RWS
import qualified Data.Array.IArray as A
import Data.Default()
import Data.Maybe
import Data.Monoid()
import qualified Data.List as L

data Emulator = Emulator
  { _emuInstr :: A.Array Addr Value
  , _emuPC :: Addr
  , _emuReg :: RegBank
  , _emuStatus :: (Bool, Bool)
  , _emuBufferD :: BufferD
  , _emuPipeW :: PipeW
  , _emuCycle0 :: Bool
  }
makeLenses ''Emulator

instance Show Emulator where
  show e = "PC: " L.++ show (e ^. emuPC) L.++ "\n"
    L.++ "Regs: " L.++ show (e ^. emuReg) L.++ "\n"
    L.++ "Status: " L.++ show (e ^. emuStatus) L.++ "\n"
    L.++ "BufferD: " L.++ show (e ^. emuBufferD) L.++ "\n"
    L.++ "PipeW: " L.++ show (e ^. emuPipeW) L.++ "\n"

withInstr :: [Instr] -> Emulator
withInstr xs = if l > 256
  then error "Too many instructions"
  else Emulator
       { _emuInstr =
           A.listArray (0, 255) (xs L.++ L.replicate (256 - l) 0)
       , _emuPC = 0
       , _emuReg = 0 :> 0 :> 0 :> 0 :> Nil
       , _emuStatus = (False, False)
       , _emuBufferD = def
       , _emuPipeW = def
       , _emuCycle0 = True
       }
  where l = L.length xs

emulate :: Emulator -> Emulator
emulate e =
  let fromFirst a = fromMaybe a . getFirst
      replaceMaybe Nothing xs = xs
      replaceMaybe (Just (i, a)) xs = replace i a xs
      bufd = e ^. emuBufferD
      w = e ^. emuPipeW
      r = PipeR
          { _cycle0 = e ^. emuCycle0
          , _counter = e ^. emuPC
          , _instr = (e ^. emuInstr) A.! (fromFirst 0 (w ^. instrA))
          , _regBank = e ^. emuReg
          }
      (bufd', (), w') = runRWS (pipeM bufd) r ()
  in e { _emuInstr = e ^. emuInstr
       , _emuPC = fromFirst 0 $ w' ^. counterW
       , _emuReg = replaceMaybe
                   ( (,)
                     <$> getFirst (w' ^. regDst)
                     <*> getFirst (w' ^. regW)
                   )
                   (e ^. emuReg)
       , _emuStatus = fromFirst (e ^. emuStatus) (w' ^. status)
       , _emuBufferD = bufd'
       , _emuPipeW = w'
       , _emuCycle0 = False
       }
