module Control.Monad.Extra
  ( bind2
  ) where

import           Control.Applicative            ( liftA2 )


bind2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
bind2 f ma mb = liftA2 (,) ma mb >>= uncurry f
