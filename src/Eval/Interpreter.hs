{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Eval.Interpreter
  ( eval
  ) where

import           Control.Applicative            ( liftA2 )
import           Control.Monad.StateT
import           Data.Functor                   ( ($>) )
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

eval' :: CoreExpr -> Result Value
eval' (Lit x       ) = return $ LitV x
eval' (Var x       ) = lookupVariable x
eval' (Lam arg body) = do
  env <- getEnv
  return $ LambdaV env arg body
eval' (App  callee        arg    ) = bind2 apply (eval' callee) (eval' arg)
eval' (Bind (name, value) context) = do
  env    <- getEnv
  value' <- eval' value
  setEnv $ Map.insert name value' env
  result <- eval' context
  setEnv env $> result

apply :: Value -> Value -> Result Value
apply (BuiltinV _ fn       ) value = Result $ liftStateM $ fn value
apply (LambdaV env arg body) value = do
  originalEnv <- getEnv
  _           <- setEnv $ Map.insert arg value env
  result      <- eval' body
  setEnv originalEnv $> result
apply expr _ = fail $ "Expression '" ++ show expr ++ "' is not callable"



lookupVariable :: Name -> Result Value
lookupVariable x = do
  env <- getEnv
  case env Map.!? x of
    Just v  -> return v
    Nothing -> fail $ "Unbound variable '" ++ x ++ "'"


-- Utilities

bind2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
bind2 f ma mb = liftA2 (,) ma mb >>= uncurry f
