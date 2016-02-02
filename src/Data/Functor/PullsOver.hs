{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | A class of 'Functor's that that be pulled over certain inner 'Functor's.

-}
module Data.Functor.PullsOver where

import           Control.Monad.Trans             (MonadTrans (..))
import           Data.Functor.Sum                (Sum (..))
import           Streaming.Prelude               (Of (..))

-- | The constraint @PullsOver f t@ indicates that it is possible to \'pull\'
-- any instance of the type constructor @f@ over an instance of the type
-- constructor @t@. This is obviously true for @'Traversable' t@ when we have
-- @'Applicative' f@, because in that case @'pullOver' = 'sequenceA'@, but
-- many more things are instances of 'PullsOver' than are 'Traversable'.
class Functor t => PullsOver f t where
  -- | Pull the inner functor over the outer.
  --
  -- > pullOver :: (Traversable t, Applicative f) => t (f a) -> f (t a)
  -- > pullOver :: (MonadTrans t, Monad m, Monad (t m)) => t m (m a) -> m (t m a)
  -- > pullOver :: (Monad m, Functor f) => Stream f m (m a) -> m (Stream f m a)
  -- > pullOver :: (Functor f, PullsOver f g, PullsOver f h) => Sum g h (f a) -> f (Sum g h a)
  -- > pullOver :: (Functor f) => Of s (f a) -> f (Of s a)
  pullOver :: t (f a) -> f (t a)

instance {-# OVERLAPPABLE #-} (MonadTrans t, Monad m, Monad (t m)) => PullsOver m (t m) where
  pullOver = return . (>>= lift)

instance {-# OVERLAPPABLE #-} (Traversable t, Applicative f) => PullsOver f t where
  pullOver = sequenceA

instance (Functor f, PullsOver f g, PullsOver f h) => PullsOver f (Sum g h) where
  pullOver (InL g) = fmap InL (pullOver g)
  pullOver (InR h) = fmap InR (pullOver h)

instance Functor f => PullsOver f (Of a) where
  pullOver (x :> ys) = fmap (x :>) ys

