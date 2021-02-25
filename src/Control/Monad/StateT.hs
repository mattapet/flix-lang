{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.StateT where

import           Control.Monad                  ( ap )
import           Data.Bifunctor                 ( first )

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

get :: (Monad m) => StateT s m s
get = StateT $ \s -> return (s, s)

put :: (Monad m) => s -> StateT s m ()
put s = StateT $ \_ -> return ((), s)

liftStateM :: (Monad m) => m a -> StateT s m a
liftStateM ma = StateT $ \s -> (, s) <$> ma

instance (Functor m) => Functor (StateT s m) where
  fmap f ma = StateT $ fmap (first f) . runStateT ma

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (<*>) = ap

instance (Monad m) => Monad (StateT s m) where
  return = pure
  ma >>= f = StateT $ \s -> do
    (a, s' ) <- runStateT ma s
    (b, s'') <- runStateT (f a) s'
    return (b, s'')

instance MonadFail (StateT s (Either String)) where
  fail = StateT . const . Left
