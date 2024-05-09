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

memory :: Monad m => Wire -> Pipe () m MemInstr
memory _ = return MemNop

memoryW :: Monad m => MemInstr -> Pipe () m ()
memoryW _ = return ()
