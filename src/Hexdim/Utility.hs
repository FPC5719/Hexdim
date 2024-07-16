module Hexdim.Utility where

import Hexdim.Datatype

import Clash.Prelude
import Control.Applicative
import Control.Lens(Lens', view)
import Data.Monoid
import Data.Maybe

pushTuple :: Applicative f
          => (f a, f b) -> f (a, b)
pushTuple (x, y) = pure (,) <*> x <*> y

replaceMaybe :: Maybe (RegSel, Value)
             -> RegBank
             -> RegBank
replaceMaybe Nothing xs = xs
replaceMaybe (Just (i, a)) xs = replace i a xs

viewFirst :: Applicative f
          => f PipeW
          -> Lens' PipeW (First a)
          -> f (Maybe a)
viewFirst w l = getFirst . view l <$> w

fromFirst :: a -> First a -> a
fromFirst x f = fromMaybe x $ getFirst f
