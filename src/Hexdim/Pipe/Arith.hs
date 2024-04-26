module Hexdim.Pipe.Arith where

import Prelude
import Control.Monad

import Hexdim.Pipe.Data

data ArithInstr = ArithInstr

arith :: Monad m => Wire -> Pipe () m ArithInstr
arith = undefined

arithW :: Monad m => ArithInstr -> Pipe () m ()
arithW = undefined
