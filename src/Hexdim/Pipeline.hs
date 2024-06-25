module Hexdim.Pipeline where

import Hexdim.Datatype
import Hexdim.Section
import Hexdim.Section.Branch
import Hexdim.Section.Memory
import Hexdim.Section.Immed
import Hexdim.Section.Arith

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
  { _bBranch :: Maybe (Buffer Branch)
  , _bMemory :: Maybe (Buffer Memory)
  , _bImmed :: Maybe (Buffer Immed)
  , _bArith :: Maybe (Buffer Arith)
  }
  deriving (Show, Generic, Default)
makeLenses ''BufferD

fetch :: Monad m
      => ForwardD
      -> Pipe m ()
fetch fwd = do
  pc <- view counter
  let pc' = fromMaybe pc $ getFirst (fwd ^. fTarget)
  scribe instrA . pure $ pc'
  scribe counterW . pure $ pc' + 1

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
    mbfBranch <- maybe (pure Nothing) (fmap pure) $
      (onDecode @Branch fwd <$> decoder @Branch rb' ins)
    mbfMemory <- maybe (pure Nothing) (fmap pure) $
      (onDecode @Memory fwd <$> decoder @Memory rb' ins)
    mbfImmed <- maybe (pure Nothing) (fmap pure) $
      (onDecode @Immed fwd <$> decoder @Immed rb' ins)
    mbfArith <- maybe (pure Nothing) (fmap pure) $
      (onDecode @Arith fwd <$> decoder @Arith rb' ins)
  
    pure ( BufferD
           (fst <$> mbfBranch)
           (fst <$> mbfMemory)
           (fst <$> mbfImmed)
           (fst <$> mbfArith)
         , mconcat . catMaybes $
           [ snd <$> mbfBranch
           , snd <$> mbfMemory
           , snd <$> mbfImmed
           , snd <$> mbfArith
           ]
         )
    else return (def, def)

execute :: Monad m
        => BufferD
        -> Pipe m ForwardE
execute buf = do
  -- Maybe (Maybe (RegSel, Value), ForwardE)
  mrfBranch <- maybe (pure Nothing) (fmap pure) $
    (onExecute @Branch <$> (buf ^. bBranch))
  mrfMemory <- maybe (pure Nothing) (fmap pure) $
    (onExecute @Memory <$> (buf ^. bMemory))
  mrfImmed <- maybe (pure Nothing) (fmap pure) $
    (onExecute @Immed <$> (buf ^. bImmed))
  mrfArith <- maybe (pure Nothing) (fmap pure) $
    (onExecute @Arith <$> (buf ^. bArith))
  
  rb <- view regBank
  let replaceMaybe Nothing xs = xs
      replaceMaybe (Just (i, a)) xs = replace i a xs
  let rchange = mconcat . L.map First . L.map join $
        [ fst <$> mrfBranch
        , fst <$> mrfMemory
        , fst <$> mrfImmed
        , fst <$> mrfArith
        ]
  let rb' = rb & replaceMaybe (getFirst rchange)
  scribe regDst $ (fst <$> rchange)
  scribe regW $ (snd <$> rchange)
  
  pure . mconcat . catMaybes $
    [ Just (def & set fReg (pure rb'))
    , snd <$> mrfBranch
    , snd <$> mrfMemory
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
  pure bufd'
