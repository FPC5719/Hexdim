{-# LANGUAGE AllowAmbiguousTypes #-}

module Hexdim.Section where

import Hexdim.Datatype

import Clash.Prelude
import Control.Lens
import Data.Default()

data ForwardD = ForwardD
  {
  }
  deriving (Generic, Default)

data ForwardE = ForwardE
  { _fStatus :: Maybe (Bool, Bool) -- zero? overflow?
  , _fReg :: Maybe RegBank
  }
  deriving (Generic, Default)
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
    => Decoded s
    -> ForwardE
    -> Pipe m ( Buffer s
              , ForwardD
              )
  onExecute
    :: Monad m
    => Buffer s
    -> Pipe m ( Maybe (RegSel, Value)
              , ForwardE
              )
