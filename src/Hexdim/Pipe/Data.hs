{-# LANGUAGE DerivingVia #-}

module Hexdim.Pipe.Data where

import Clash.Prelude

import Control.Monad.RWS
import Control.Lens
import Data.Monoid.Generic

type Wire = Unsigned 8
type Addr = Unsigned 8
type Reg  = Unsigned 2

data PipeR = PipeR
  { _cycle0 :: Bool
  , _pc :: Wire

  , _instrMemR :: Wire
  , _dataMemR  :: Maybe Wire
  , _ioR       :: Maybe Wire

  , _regR1     :: Wire
  , _regR2     :: Wire

  , _statusR   :: (Bool, Bool)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''PipeR

data PipeW = PipeW
  -- Nothing: +1, Just a: Jump
  { _pcNext    :: First (Maybe Addr)
  
  , _instrMemA :: First Addr
  -- Left: Read, Right: Write
  , _dataMemW  :: First (Either Addr (Addr, Wire))
  , _ioW       :: First (Either Addr (Addr, Wire))
  
  , _regS1     :: First Reg
  , _regS2     :: First Reg
  , _regW      :: First (Maybe (Reg, Wire))

  , _statusW   :: First (Maybe (Bool, Bool))
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
  deriving Semigroup via GenericSemigroup PipeW
  deriving Monoid via GenericMonoid PipeW
makeLenses ''PipeW

type Pipe s m a = RWST PipeR PipeW s m a
