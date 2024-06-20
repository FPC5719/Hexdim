{-# LANGUAGE DerivingVia #-}

module Hexdim.Datatype where

import Clash.Prelude
import Control.Lens
import Control.Monad.RWS
import Data.Monoid.Generic

type Value  = Unsigned 8
type Instr  = Unsigned 8
type Addr   = Unsigned 8
type RegSel = Unsigned 2
type Immed  = Unsigned 4

type RegBank = Vec 4 Value

data PipeR = PipeR
  { _cycle0 :: Bool
  , _counter :: Addr
  , _instr :: Instr
  , _regBank :: RegBank
  }
makeLenses ''PipeR

data PipeW = PipeW
  { _counterW :: First Addr
  , _instrA :: First Addr
  , _status :: First (Bool, Bool)
  , _regDst :: First RegSel
  , _regW :: First Value
  }
  deriving (Generic, Eq, Show, Default)
  deriving Semigroup via GenericSemigroup PipeW
  deriving Monoid via GenericMonoid PipeW
makeLenses ''PipeW

type Pipe m a = RWST PipeR PipeW () m a
