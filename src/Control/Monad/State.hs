{-# LANGUAGE TupleSections #-}

module Control.Monad.State where

import           Control.Applicative            ( liftA )
import           Control.Monad                  ( ap )

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State a) where
  fmap = liftA

instance Applicative (State a) where
  pure a = State (a, )
  (<*>) = ap

instance Monad (State a) where
  return = pure
  ma >>= f = State $ \s -> let (a, s') = runState ma s in runState (f a) s'
