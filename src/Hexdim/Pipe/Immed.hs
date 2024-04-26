module Hexdim.Pipe.Immed where

import Clash.Prelude
import Control.Lens

import Hexdim.Pipe.Data

immed :: Monad m => Wire -> Pipe () m ()
immed = \case
  $(bitPattern "10rrmmmm") -> do
    scribe regSW $ pure (Just rr)
    scribe regW $ pure (Just (bitCoerce (0 ++# mmmm)))
  _ -> return ()
