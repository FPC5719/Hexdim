module Hexdim.Pipe.Branch where

import Prelude
import Control.Monad

import Hexdim.Pipe.Data

branch :: Monad m => Wire -> Pipe StageID () m ()
branch _ = return ()

branchW :: Monad m => () -> Pipe StageEX () m ()
branchW = pure
