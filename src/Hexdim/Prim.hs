{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Hexdim.Prim where

import Clash.Prelude

import Control.Lens
import Control.Monad
import Control.Monad.RWS
import Data.Monoid.Generic
import Data.Maybe

type Wire = Unsigned 8

data PipeR = PipeR
  { _cycle0       :: Bool

  , _fromInstrMem :: Wire
  , _fromDataMem  :: Wire
  , _fromIO       :: Wire
  , _fromRegs1    :: Wire
  , _fromRegs2    :: Wire }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX)
makeLenses ''PipeR

data PipeW = PipeW
  { _addrInstrMem :: First Wire
  
  , _addrDataMem  :: First Wire
  , _toDataMem    :: First (Maybe Wire)
  
  , _addrIO       :: First Wire
  , _toIO         :: First (Maybe Wire)

  , _selRegs1     :: First Wire
  , _selRegs2     :: First Wire
  , _selRegs      :: First Wire
  , _toRegs       :: First (Maybe Wire) }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX)
  deriving Semigroup via GenericSemigroup PipeW
  deriving Monoid via GenericMonoid PipeW
makeLenses ''PipeW

data StageIFS = StageIFS
  { _pc :: Wire }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX)
makeLenses ''StageIFS

type StageIDS = ()

type StageMAS = ()

data StageEXS = StageEXS
  { _isZero     :: Bool
  , _isOverflow :: Bool }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX)
makeLenses ''StageIFS

type StageWBS = ()

type StoreIFID = ()

type StoreIDMA = ()

type StoreMAEX = ()

type StoreEXWB = ()

data PipeS = PipeS
  { _stageIFS :: StageIFS
  , _stageIDS :: StageIDS
  , _stageMAS :: StageMAS
  , _stageEXS :: StageEXS
  , _stageWBS :: StageWBS
  , _storeIFID :: StoreIFID
  , _storeIDMA :: StoreIDMA
  , _storeMAEX :: StoreMAEX
  , _storeEXWB :: StoreEXWB }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX)
makeLenses ''PipeS

initState :: PipeS
initState = PipeS
  { _stageIFS = StageIFS
    { _pc = 0 }
  , _stageIDS = ()
  , _stageMAS = ()
  , _stageEXS = StageEXS
    { _isZero = False
    , _isOverflow = False }
  , _stageWBS = ()
  , _storeIFID = ()
  , _storeIDMA = ()
  , _storeMAEX = ()
  , _storeEXWB = () }

type Pipe s m a = RWST PipeR PipeW s m a

stageIF :: () -> Pipe StageIFS m StoreIFID
stageIF = undefined

stageID :: StoreIFID -> Pipe StageIDS m StoreIDMA
stageID = undefined

stageMA :: StoreIDMA -> Pipe StageMAS m StoreMAEX
stageMA = undefined

stageEX :: StoreMAEX -> Pipe StageEXS m StoreEXWB
stageEX = undefined

stageWB :: StoreEXWB -> Pipe StageWBS m ()
stageWB = undefined

store :: a -> Pipe a m a
store x = get >>= \x0 -> put x >> return x0

pipeM :: Pipe PipeS m ()
pipeM =                        zoom stageIFS stageIF
  >=> zoom storeIFID store >=> zoom stageIDS stageID
  >=> zoom storeIDMA store >=> zoom stageMAS stageMA
  >=> zoom storeMAEX store >=> zoom stageEXS stageEX
  >=> zoom storeEXWB store >=> zoom stageWBS stageWB

-- Miscellaneous
fromFirst :: a -> First a -> a
fromFirst x = fromMaybe x . getFirst
