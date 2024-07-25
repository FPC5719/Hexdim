module Hexdim.Core where

import Hexdim.Datatype
import Hexdim.Utility
import Hexdim.Pipeline

import Clash.Prelude
import Control.Monad.RWS
import Control.Lens(makeLenses, (^.))
import qualified Data.List as L

data CoreS = CoreS
  { _coreCycle0 :: Bool
  , _coreReg    :: RegBank
  , _coreStatus :: (Bool, Bool)
  , _corePC     :: Addr
  , _corePipeS  :: PipeS
  }
  deriving (Generic, NFDataX)
makeLenses ''CoreS

instance Show CoreS where
  show s = "CoreS:\n"
    L.++ "  Reg: " L.++ show (s ^. coreReg) L.++ "\n"
    L.++ "  Status: " L.++ show (s ^. coreStatus) L.++ "\n"
    L.++ "  PC: " L.++ show (s ^. corePC) L.++ "\n"

instance Default CoreS where
  def = CoreS
    { _coreCycle0 = True
    , _coreReg    = 0 :> 0 :> 0 :> 0 :> Nil
    , _coreStatus = (False, False)
    , _corePC     = 0
    , _corePipeS  = def
    }

data CoreR = CoreR
  { _coreInstrR :: Instr
  , _coreDataR  :: Value
  }
makeLenses ''CoreR

data CoreW = CoreW
  { _coreInstrA :: Addr
  , _coreDataA  :: Addr
  , _coreDataW  :: Value
  , _coreSelRW  :: Bool -- False: Read, True: Write
  , _coreSelMP  :: Bool -- False: Memory, True: Peripheral
  , _coreSelEn  :: Bool -- False: Ignore the above two
  }
makeLenses ''CoreW

instance Default CoreW where
  def = CoreW
    { _coreInstrA = 0
    , _coreDataA  = 0
    , _coreDataW  = 0
    , _coreSelRW  = False
    , _coreSelMP  = False
    , _coreSelEn  = False
    }

core :: CoreS -> CoreR -> (CoreS, CoreW)
core cs cr = (cs', cw)
  where cs' = CoreS
          { _coreCycle0 = False
          , _coreReg    = replaceMaybe
                          ( (,)
                            <$> getFirst (pw ^. regDst)
                            <*> getFirst (pw ^. regW)
                          )
                          (cs ^. coreReg)
          , _coreStatus = fromFirst (cs ^. coreStatus) (pw ^. statusW)
          , _corePC     = fromFirst (cs ^. corePC) (pw ^. counterW)
                          -- `counterW` should never be `Nothing`
          , _corePipeS  = ps'
          }
        cw = CoreW
          { _coreInstrA = fromFirst 0 (pw ^. instrA)
          , _coreDataA  = da
          , _coreDataW  = dw
          , _coreSelRW  = srw
          , _coreSelMP  = smp
          , _coreSelEn  = sen
          }
        pr = PipeR
          { _cycle0  = cs ^. coreCycle0
          , _counter = cs ^. corePC
          , _instr   = cr ^. coreInstrR
          , _regBank = cs ^. coreReg
          , _statusR = cs ^. coreStatus
          , _memoryR = cr ^. coreDataR
          , _periR   = cr ^. coreDataR
          }
        (ps', (), pw) = runRWS (pipeM (cs ^. corePipeS)) pr ()
        (da, dw, srw, smp, sen)
          = case ( getFirst (pw ^. memoryA)
                 , getFirst (pw ^. memoryW)
                 , getFirst (pw ^. periA)
                 , getFirst (pw ^. periW)
                 )
            of (Nothing, _      , Nothing, _      ) ->
                 (0, 0, False, False, False)
               (Nothing, _      , Just a , Nothing) ->
                 (a, 0, False, True , True )
               (Nothing, _      , Just a , Just d ) ->
                 (a, d, True , True , True )
               (Just a , Nothing, _      , _      ) ->
                 (a, 0, False, False, True )
               (Just a , Just d , _      , _      ) ->
                 (a, d, True , False, True )

