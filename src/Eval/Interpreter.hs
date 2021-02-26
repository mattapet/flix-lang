{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Eval.Interpreter
  ( eval
  ) where

import           Control.Applicative            ( (<|>)
                                                , liftA2
                                                )
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

eval' (Case value patterns) = do
  value' <- eval' value
  result <- patternMatch patterns value'
  maybe (fail "failure") return result

apply :: Value -> Value -> Result Value
apply (BuiltinV _ fn       ) value = Result $ liftStateM $ fn value
apply (LambdaV env arg body) value = do
  originalEnv <- getEnv
  setEnv $ Map.insert arg value env <> originalEnv
  result <- eval' body
  setEnv originalEnv $> result
apply expr _ = fail $ "Expression '" ++ show expr ++ "' is not callable"

patternMatch :: [Pattern] -> Value -> Result (Maybe Value)
patternMatch [] _ = return Nothing
patternMatch (x : xs) value =
  liftA2 (<|>) (patternMatch' x value) (patternMatch xs value)

patternMatch' :: Pattern -> Value -> Result (Maybe Value)
patternMatch' (DefaultP result) _ = Just <$> eval' result
-- Matching literals
patternMatch' (LitP (Int x) result) (LitV (Int y))
  | x == y    = Just <$> eval' result
  | otherwise = return Nothing
patternMatch' (LitP (Bool x) result) (LitV (Bool y))
  | x == y    = Just <$> eval' result
  | otherwise = return Nothing
patternMatch' _ _ = return Nothing

lookupVariable :: Name -> Result Value
lookupVariable x = do
  env <- getEnv
  case env Map.!? x of
    Just v  -> return v
    Nothing -> fail $ "Unbound variable '" ++ x ++ "'"


-- Utilities

bind2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
bind2 f ma mb = liftA2 (,) ma mb >>= uncurry f
