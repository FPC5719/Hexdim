module Hexdim.Prim where

import Prelude

import Hexdim.Aux.Pipe
import Hexdim.Pipe.Data
import Hexdim.Pipe.Fetch
import Hexdim.Pipe.Branch
import Hexdim.Pipe.Memory
import Hexdim.Pipe.Immed
import Hexdim.Pipe.Arith

pipeM :: Monad m => () -> Pipe PipeState m ()
pipeM = fetch +>> branch
              *>> (memory +>> memoryW)
              *>> immed
              *>> (arith +>> arithW)

-- How to reduce the boilerplate?
type PipeState =
  (Wire, ((), ((MemInstr, ()), ((), (ArithInstr, ())))))

zipPipeState :: PipeState
             -> (Wire, MemInstr, ArithInstr)
zipPipeState (w, ((), ((m, ()), ((), (a, ()))))) =
  (w, m, a)

unzipPipeState :: (Wire, MemInstr, ArithInstr)
               -> PipeState
unzipPipeState (w, m, a) =
  (w, ((), ((m, ()), ((), (a, ())))))
