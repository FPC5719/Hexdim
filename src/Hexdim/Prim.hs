module Hexdim.Prim where

import Prelude

import Hexdim.Aux.Pipe
import Hexdim.Pipe.Data
import Hexdim.Pipe.Fetch
import Hexdim.Pipe.Branch
import Hexdim.Pipe.Memory
import Hexdim.Pipe.Immed
import Hexdim.Pipe.Arith

pipeM :: Monad m => () -> Pipe
  (Wire, ((), ((MemInstr, ()), ((), (ArithInstr, ())))))
  m ()
pipeM = fetch +>> branch
              *>> (memory +>> memoryW)
              *>> immed
              *>> (arith +>> arithW)
