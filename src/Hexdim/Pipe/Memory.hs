module Hexdim.Pipe.Memory where

import Prelude
import Control.Monad
import Data.Default

import Hexdim.Pipe.Data

data MemInstr = MemNop

instance Default MemInstr where
  def = MemNop

memory :: Wire -> Pipe () m MemInstr
memory = undefined

memoryW :: MemInstr -> Pipe () m ()
memoryW = undefined
