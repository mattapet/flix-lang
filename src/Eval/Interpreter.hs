{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval.Interpreter
  ( eval
  ) where

import           Control.Applicative            ( (<|>)
                                                , liftA2
                                                )
import           Control.Monad.Extra            ( bind2 )
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

eval' (Bind (name, value) context) = pushFrame $ do
  eval' value >>= bindValue name
  eval' context

eval' (Case value patterns) = do
  value' <- eval' value
  result <- patternMatch patterns value'
  maybe (fail "failure") return result

apply :: Value -> Value -> Result Value
apply (BuiltinV _ fn       ) value = Result $ liftStateM $ fn value
apply (LambdaV env arg body) value = pushFrame $ switchContext env $ do
  bindValue arg value
  eval' body
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
  unpack $ env Map.!? x
  where
    unpack (Just v) = return v
    unpack Nothing  = fail $ "Unbound variable '" ++ x ++ "'"

bindValue :: Name -> Value -> Result ()
bindValue name value = getEnv >>= setEnv . Map.insert name value

pushFrame :: Result a -> Result a
pushFrame f = do
  env    <- getEnv
  result <- f
  setEnv env $> result

switchContext :: Environment -> Result a -> Result a
switchContext env f = pushFrame $ do
  baseEnv <- getEnv
  setEnv (env <> baseEnv) >> f
