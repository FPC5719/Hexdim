module Hexdim.Pipeline where

import Hexdim.Datatype
import Hexdim.Section
import Hexdim.Section.Arith

import Clash.Prelude
import Control.Lens
  ( makeLenses, scribe, set, view
  , (^.), (&)
  )
import Control.Monad
import Data.Maybe
import Data.Monoid

data BufferD = BufferD
  { _bArith :: Maybe (Buffer Arith)
  }
makeLenses ''BufferD

fetch :: Monad m
      => ForwardD
      -> Pipe m ()
fetch fwd = do
  pc <- view counter
  scribe instrA . pure $ pc
  scribe counterW . pure $ pc + 1

decode :: Monad m
       => ForwardE
       -> Pipe m (BufferD, ForwardD)
decode fwd = do
  rb <- view regBank
  let rb' = fromMaybe rb (getFirst (fwd ^. fReg))
  ins <- view instr

  -- Maybe (Buffer s, ForwardD)
  mbfArith <- maybe (pure Nothing) (fmap pure) $
    (onDecode @Arith fwd <$> decoder @Arith rb' ins)
  
  return ( BufferD
           (fst <$> mbfArith)
         , mconcat . catMaybes $
           [ (snd <$> mbfArith)
           ]
         )

execute :: Monad m
        => BufferD
        -> Pipe m ForwardE
execute buf = do
  -- Maybe (Maybe (RegSel, Value), ForwardE)
  mrfArith <- maybe (pure Nothing) (fmap pure) $
    (onExecute @Arith <$> (buf ^. bArith))
  
  rb <- view regBank
  let replaceMaybe Nothing xs = xs
      replaceMaybe (Just (i, a)) xs = replace i a xs
  let rb' = rb &
        replaceMaybe (join (fst <$> mrfArith))
  
  return . mconcat . catMaybes $
    [ Just (def & set fReg (pure rb'))
    , snd <$> mrfArith
    ]

pipeM :: Monad m
      => BufferD
      -> Pipe m BufferD
pipeM bufd = do
  fe <- execute bufd
  (bufd', fd) <- decode fe
  fetch fd
  return bufd'
