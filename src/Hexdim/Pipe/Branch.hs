module Hexdim.Pipe.Branch where

import Prelude
import Control.Monad

import Hexdim.Pipe.Data

branch :: Monad m => Wire -> Pipe () m ()
branch _ = return ()
