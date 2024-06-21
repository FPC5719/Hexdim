module Hexdim.Pipeline where

import Hexdim.Datatype
import Hexdim.Section
import Hexdim.Section.Arith
import Hexdim.Section.Immed

import Clash.Prelude
import Control.Lens
  ( makeLenses, scribe, set, view
  , (^.), (&)
  )
import Control.Monad
import Data.Maybe
import Data.Monoid
import qualified Data.List as L

data BufferD = BufferD
  { _bImmed :: Maybe (Buffer Immed)
  , _bArith :: Maybe (Buffer Arith)
  }
  deriving (Show, Generic, Default)
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
  f <- view cycle0
  if not f
    then do
    rb <- view regBank
    let rb' = fromMaybe rb (getFirst (fwd ^. fReg))
    ins <- view instr

    -- Maybe (Buffer s, ForwardD)
    mbfImmed <- maybe (pure Nothing) (fmap pure) $
      (onDecode @Immed fwd <$> decoder @Immed rb' ins)
    mbfArith <- maybe (pure Nothing) (fmap pure) $
      (onDecode @Arith fwd <$> decoder @Arith rb' ins)
  
    return ( BufferD
             (fst <$> mbfImmed)
             (fst <$> mbfArith)
           , mconcat . catMaybes $
             [ snd <$> mbfImmed
             , snd <$> mbfArith
             ]
           )
    else return (def, def)

execute :: Monad m
        => BufferD
        -> Pipe m ForwardE
execute buf = do
  -- Maybe (Maybe (RegSel, Value), ForwardE)
  mrfImmed <- maybe (pure Nothing) (fmap pure) $
    (onExecute @Immed <$> (buf ^. bImmed))
  mrfArith <- maybe (pure Nothing) (fmap pure) $
    (onExecute @Arith <$> (buf ^. bArith))
  
  rb <- view regBank
  let replaceMaybe Nothing xs = xs
      replaceMaybe (Just (i, a)) xs = replace i a xs
  let rchange = mconcat . L.map First . L.map join $
        [ fst <$> mrfImmed
        , fst <$> mrfArith
        ]
  let rb' = rb & replaceMaybe (getFirst rchange)
  scribe regDst $ (fst <$> rchange)
  scribe regW $ (snd <$> rchange)
  
  return . mconcat . catMaybes $
    [ Just (def & set fReg (pure rb'))
    , snd <$> mrfImmed
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
