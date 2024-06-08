module Hexdim.Pipe.Memory where

import Prelude
import Control.Monad
import Data.Default

import Hexdim.Pipe.Data

data MemInstr
  = MemNop
  deriving (Show, Eq)

instance Default MemInstr where
  def = MemNop

memory :: Monad m => Wire -> Pipe StageID () m MemInstr
memory _ = return MemNop

memoryW :: Monad m => MemInstr -> Pipe StageEX () m ()
memoryW _ = return ()
