module Hexdim.Pipe.Immed where

import Clash.Prelude
import Control.Lens

import Hexdim.Pipe.Data

immed :: Monad m => Wire -> Pipe StageID () m ()
immed = \case
  $(bitPattern "10rrmmmm") ->
    scribe regW . pure $
      (bitCoerce rr, bitCoerce (0 ++# mmmm))
  _ -> return ()

immedW :: Monad m => () -> Pipe StageEX () m ()
immedW = pure
