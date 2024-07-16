{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}

module Hexdim.Section where

import Hexdim.Datatype

import Clash.Prelude
import Control.Lens
import Data.Default()
import Data.Monoid
import Data.Monoid.Generic

data ForwardD = ForwardD
  { _fTarget :: First Addr
  }
  deriving (Generic, Default)
  deriving Semigroup via GenericSemigroup ForwardD
  deriving Monoid via GenericMonoid ForwardD
makeLenses ''ForwardD

data ForwardE = ForwardE
  { _fStatus :: First (Bool, Bool) -- zero? overflow?
  , _fReg :: First RegBank
  }
  deriving (Generic, Default)
  deriving Semigroup via GenericSemigroup ForwardE
  deriving Monoid via GenericMonoid ForwardE
makeLenses ''ForwardE

class Section s where
  type Decoded s
  type Buffer s

  decoder
    :: RegBank
    -> Instr
    -> Maybe (Decoded s)
  onDecode
    :: Monad m
    => ForwardE
    -> Decoded s
    -> Pipe m ( Buffer s
              , ForwardD
              )
  onExecute
    :: Monad m
    => Buffer s
    -> Pipe m ( Maybe (RegSel, Value)
              , ForwardE
              )
