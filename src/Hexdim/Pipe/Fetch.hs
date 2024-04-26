module Hexdim.Pipe.Fetch where

import Clash.Prelude

import Control.Lens
import Control.Monad.Extra

import Hexdim.Pipe.Data

fetch :: Monad m => () -> Pipe () m Wire
fetch () = do
  curpc <- view pc
  scribe instrMemA (pure curpc)
  scribe pcNext (pure Nothing)
  ifM (view cycle0)
    (return 0)
    (view instrMemR)
