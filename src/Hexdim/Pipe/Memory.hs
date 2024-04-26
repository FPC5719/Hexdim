module Hexdim.Pipe.Memory where

import Prelude
import Control.Monad

import Hexdim.Pipe.Data

data MemInstr = MemInstr

memory :: Wire -> Pipe () m MemInstr
memory = undefined

memoryW :: MemInstr -> Pipe () m ()
memoryW = undefined
