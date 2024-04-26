module Hexdim.Aux.Pipe where

import Control.Monad.RWS
import Prelude

infixr 2 +>>
infixr 3 *>>

(+>>) :: (Monad m, Monoid w)
      => (a -> RWST r w () m b)
      -> (b -> RWST r w s m c)
      -> (a -> RWST r w (b, s) m c)
(+>>) f g = \a -> RWST $ \r (b0, s0) -> do
  (b, (), w) <- runRWST (f a) r ()
  (c, s, w') <- runRWST (g b0) r s0
  return (c, (b, s), w' `mappend` w)

(*>>) :: (Monad m, Monoid w)
      => (a -> RWST r w s1 m ())
      -> (a -> RWST r w s2 m ())
      -> (a -> RWST r w (s1, s2) m ())
(*>>) f g = \a -> RWST $ \r (s1, s2) -> do
  ((), s1', w1) <- runRWST (f a) r s1
  ((), s2', w2) <- runRWST (g a) r s2
  return ((), (s1', s2'), w1 `mappend` w2)
