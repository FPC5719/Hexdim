module Hexdim.Emulator where

import Hexdim.Datatype
import Hexdim.Core

import Clash.Prelude
import Control.Lens(makeLenses, (^.))
import qualified Data.Array.IArray as A
import qualified Data.List as L

data Emulator = Emulator
  { _emuInstr :: A.Array Addr Value
  , _emuMemory :: A.Array Addr Value
  , _emuPeri :: Maybe (Addr, Value)
  , _emuCoreS :: CoreS
  , _emuLastW :: CoreW
  }
makeLenses ''Emulator

instance Show Emulator where
  show e = "Peripheral: " L.++ show (e ^. emuPeri) L.++ "\n"
    L.++ show (e ^. emuCoreS)

withInstr :: [Instr] -> Emulator
withInstr xs = if l > 256
  then error "Too many instructions"
  else Emulator
       { _emuInstr =
           A.listArray (0, 255) (xs L.++ L.replicate (256 - l) 0)
       , _emuMemory =
           A.listArray (0, 255) (L.replicate 256 0)
       , _emuPeri = Nothing
       , _emuCoreS = def
       , _emuLastW = def
       }
  where l = L.length xs

emulate :: Emulator -> Emulator
emulate e = Emulator
  { _emuInstr = e ^. emuInstr
  , _emuMemory =
      case ( cw ^. coreSelRW
           , cw ^. coreSelMP
           , cw ^. coreSelEn)
      of (True, False, True) ->
           (e ^. emuMemory) A.// [ (cw ^. coreDataA
                                   , cw ^. coreDataW
                                   )
                                 ]
         _ -> e ^. emuMemory
  , _emuPeri =
    case ( cw ^. coreSelRW
         , cw ^. coreSelMP
         , cw ^. coreSelEn)
    of (True, True, True) ->
         Just ( cw ^. coreDataA
              , cw ^. coreDataW
              )
       _ -> Nothing
  , _emuCoreS = cs'
  , _emuLastW = cw'
  }
  where
    cs = e ^. emuCoreS
    cw = e ^. emuLastW
    cr = CoreR { _coreInstrR =
                   (e ^. emuInstr) A.! (cw ^. coreInstrA)
               , _coreDataR =
                   case ( cw ^. coreSelRW
                        , cw ^. coreSelMP
                        , cw ^. coreSelEn)
                   of (_, _, False) -> 0
                      (True, _, True) -> 0
                      (False, False, True) ->
                        (e ^. emuMemory) A.! (cw ^. coreDataA)
                      (False, True, True) -> 0
                        -- Ignore Peripheral input (For the time being)
               }
    (cs', cw') = core cs cr
