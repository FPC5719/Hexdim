module Hexdim.Utility where

import Hexdim.Datatype

import Clash.Prelude
import Control.Lens(Lens', view)
import Data.Monoid
import Data.Maybe

replaceMaybe :: Maybe (RegSel, Value)
             -> RegBank
             -> RegBank
replaceMaybe Nothing       = id
replaceMaybe (Just (i, a)) = replace i a

viewFirst :: Functor f
          => f PipeW
          -> Lens' PipeW (First a)
          -> f (Maybe a)
viewFirst w l = getFirst . view l <$> w

fromFirst :: a -> First a -> a
fromFirst x = fromMaybe x . getFirst
