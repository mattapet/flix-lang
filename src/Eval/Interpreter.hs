{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Eval.Interpreter
  ( eval
  ) where

import           Control.Applicative            ( liftA2 )
import           Control.Monad.StateT
import qualified Data.Map                      as Map
import           Eval.Core

newtype Result a = Result { runResult :: StateT Environment (Either String) a }
  deriving (Monad, Applicative, Functor)

instance MonadFail Result where
  fail = Result . liftStateM . Left

getEnv :: Result Environment
getEnv = Result get

setEnv :: Environment -> Result ()
setEnv = Result . put

eval :: Environment -> CoreExpr -> Either String (Value, Environment)
eval env = flip runStateT env . runResult . eval'
  where
    eval' :: CoreExpr -> Result Value
    eval' (Lit x         ) = return $ LitV x
    eval' (Var x         ) = lookupVariable x
    eval' (App callee arg) = bind2 apply (eval' callee) (eval' arg)

apply :: Value -> Value -> Result Value
apply (BuiltinV _ fn) value = Result $ liftStateM $ fn value


lookupVariable :: Name -> Result Value
lookupVariable x = do
  env <- getEnv
  case env Map.!? x of
    Just v  -> return v
    Nothing -> fail $ "Unbound variable '" ++ x ++ "'"


-- Utilities

bind2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
bind2 f ma mb = liftA2 (,) ma mb >>= uncurry f
