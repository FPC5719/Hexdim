{-# LANGUAGE DerivingVia #-}

module Hexdim.Pipe.Data where

import Clash.Prelude

import Control.Monad.RWS
import Control.Lens
import Data.Monoid.Generic

type Wire = Unsigned 8

data PipeR = PipeR
  { _emptyR :: Wire }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''PipeR

data PipeW = PipeW
  { _emptyW :: First Wire }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
  deriving Semigroup via GenericSemigroup PipeW
  deriving Monoid via GenericMonoid PipeW
makeLenses ''PipeW

type Pipe s m a = RWST PipeR PipeW s m a
