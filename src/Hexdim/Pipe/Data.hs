{-# LANGUAGE DerivingVia #-}

module Hexdim.Pipe.Data where

import Clash.Prelude

import Control.Monad.RWS
import Control.Lens (makeLenses)
import Data.Monoid.Generic

type Wire = Unsigned 8
type Addr = Unsigned 8
type Reg  = Unsigned 2
type Imm  = Unsigned 4

data StageTag
  = StageIF
  | StageID
  | StageEX

{-
type family StageIndex (a :: StageTag) where
  StageIndex StageIF = 0
  StageIndex StageID = 1
  StageIndex StageEX = 2

type family NextStage (a :: StageTag) where
  NextStage StageIF = StageID
  NextStage StageID = StageEX
-}

data PipeR = PipeR
  { _cycle0 :: Bool
  , _pc :: Wire

  , _instrMemR :: Wire
  , _dataMemR  :: Maybe Wire
  , _ioR       :: Maybe Wire

  , _regR1     :: Wire
  , _regR2     :: Wire

  , _statusR   :: (Bool, Bool)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''PipeR

data PipeW = PipeW
  -- Nothing: +1, Just a: Jump
  { _pcNext    :: First Addr
  
  , _instrMemA :: First Addr
  -- Left: Read, Right: Write
  , _dataMemW  :: First (Either Addr (Addr, Wire))
  , _ioW       :: First (Either Addr (Addr, Wire))
  
  , _regS1     :: First Reg
  , _regS2     :: First Reg
  , _regW      :: First (Reg, Wire)
  
  , _statusW   :: First (Bool, Bool)
  }
  deriving stock (Generic, Show, Eq)n
  deriving anyclass NFDataX
  deriving Semigroup via GenericSemigroup PipeW
  deriving Monoid via GenericMonoid PipeW
makeLenses ''PipeW

-- Finally the simplest RWST?
type Pipe s m a b = a -> RWST PipeR PipeW s m b



-- The following do not work for now

{-
type PipeSection (l :: StageTag) (r :: StageTag) s m a
  = RWST PipeR (Vec (StageIndex r - StageIndex l + 1) PipeW) s m a

type Pipe (t :: StageTag) s m a = RWST PipeR PipeW s m a

toSection :: Monad m => Pipe t s m a -> PipeSection t t s m a
toSection p = RWST $ \r s -> do
  (a, s', w) <- runRWST p r s
  return (a, s', w :> Nil)
-}
{-
infixr 2 +>>, +>>|
infixr 3 *>>

(+>>) :: Monad m
      => StageIndex (NextStage t) <= StageIndex r
      => StageIndex (NextStage t) ~ StageIndex t + 1
      => (a -> Pipe t () m b)
      -> (b -> PipeSection (NextStage t) r s m c)
      -> (a -> PipeSection t r (b, s) m c)
(+>>) f g = \a -> RWST $ \r (b0, s0) -> do
  (b, (), w) <- runRWST (f a) r ()
  (c, s, w') <- runRWST (g b0) r s0
  return (c, (b, s), w :> w')

(+>>|) :: Monad m
       => StageIndex (NextStage t) ~ StageIndex t + 1
       => (a -> Pipe t () m b)
       -> (b -> Pipe (NextStage t) () m c)
       -> (a -> PipeSection t (NextStage t) b m c)
(+>>|) f g = \a -> RWST $ \r b0 -> do
  (b, (), w) <- runRWST (f a) r ()
  (c, (), w') <- runRWST (g b0) r ()
  return (c, b, w :> w' :> Nil)

(*>>) :: Monad m
      => StageIndex l <= StageIndex r
      => KnownNat (StageIndex l)
      => KnownNat (StageIndex r)
      => (a -> PipeSection l r s1 m ())
      -> (a -> PipeSection l r s2 m ())
      -> (a -> PipeSection l r (s1, s2) m ())
(*>>) f g = \a -> RWST $ \r (s1, s2) -> do
  ((), s1', w1) <- runRWST (f a) r s1
  ((), s2', w2) <- runRWST (g a) r s2
  return ((), (s1', s2'), w1 `mappend` w2)
-}
