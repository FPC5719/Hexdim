module Hexdim.Pipeline where

-- If you decide to add a new section,
-- modify the following 8 places.
-- (Perhaps it can be done with TH)

import Hexdim.Datatype
import Hexdim.Utility
import Hexdim.Section
import Hexdim.Section.Branch
import Hexdim.Section.Memory
import Hexdim.Section.Immed
import Hexdim.Section.Arith
-- import Hexdim.Section.<...> -- (1)

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
  , _bImmed  :: Maybe (Buffer Immed)
  , _bArith  :: Maybe (Buffer Arith)
  -- , _b<...> :: Maybe (Buffer <...>) -- (2)
  }
  deriving (Show, Generic, Default, NFDataX)
makeLenses ''BufferD

type PipeS = BufferD

fetch :: Monad m
      => ForwardD
      -> Pipe m ()
fetch fwd = do
  f <- view cycle0
  if not f
    then do
    pc <- view counter
    case getFirst (fwd ^. fTarget) of
      Nothing -> do
        scribe instrA . pure $ pc
        scribe counterW . pure $ pc + 1
      Just target -> do
        scribe instrA . pure $ target
        scribe counterW . pure $ target + 1
    else scribe instrA . pure $ 0

decode :: Monad m
       => ForwardE
       -> Pipe m (BufferD, ForwardD)
decode fwd = do
  f <- view cycle0
  if not f
    then do
    rb <- view regBank
    let rb' = fromFirst rb (fwd ^. fReg)
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
    -- mbf<...> <- maybe (pure Nothing) (fmap pure) $
    --   (onDecode @<...> fwd <$> decoder @<...> rb' ins) -- (3)
  
    pure ( BufferD
           (fst <$> mbfBranch)
           (fst <$> mbfMemory)
           (fst <$> mbfImmed)
           (fst <$> mbfArith)
           -- (fst <$> mbf<...>) -- (4)
         , mconcat . catMaybes $
           [ snd <$> mbfBranch
           , snd <$> mbfMemory
           , snd <$> mbfImmed
           , snd <$> mbfArith
           -- , snd <$> mbf<...> -- (5)
           ]
         )
    else pure (def, def)

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
  -- mrf<...> <- maybe (pure Nothing) (fmap pure) $
  --   (onExecute @<...> <$> (buf ^. b<...>)) -- (6)
  
  rb <- view regBank
  let rchange = mconcat . L.map First . L.map join $
        [ fst <$> mrfBranch
        , fst <$> mrfMemory
        , fst <$> mrfImmed
        , fst <$> mrfArith
        -- , fst <$> mrf<...> -- (7)
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
    -- , snd <$> mrf<...> -- (8)
    ]

pipeM :: Monad m
      => PipeS
      -> Pipe m PipeS
pipeM bufd = do
  fe <- execute bufd
  (bufd', fd) <- decode fe
  fetch fd
  pure bufd'
