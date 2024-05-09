module Hexdim.Pipe.Immed where

import Clash.Prelude
import Control.Lens

import Hexdim.Pipe.Data

immed :: Monad m => Wire -> Pipe () m ()
immed = \case
  $(bitPattern "10rrmmmm") ->
    scribe regW . pure $
      Just (bitCoerce rr, bitCoerce (0 ++# mmmm))
  _ -> return ()
