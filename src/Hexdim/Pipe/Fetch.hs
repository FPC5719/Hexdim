module Hexdim.Pipe.Fetch where

import Clash.Prelude

import Control.Lens
import Control.Monad.Extra

import Hexdim.Pipe.Data

fetch :: Monad m => () -> Pipe StageIF () m Wire
fetch () = do
  curpc <- view pc
  scribe instrMemA (pure curpc)
  ifM (view cycle0)
    (return 0)
    (view instrMemR)
