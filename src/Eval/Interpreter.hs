{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval.Interpreter
  ( eval
  ) where

import           Control.Applicative            ( (<|>)
                                                , Alternative(..)
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

instance Alternative Result where
  empty = fail ""
  Result x <|> Result y = Result $ StateT $ \s -> case runStateT x s of
    val@Right{} -> val
    Left _      -> runStateT y s

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

eval' (Case value patterns) = pushFrame $ do
  value' <- eval' value
  result <- patternMatch patterns value'
  maybe (fail "Pattern match fallthrough") eval' result

-- Lambda applications

apply :: Value -> Value -> Result Value
apply (BuiltinV _ fn       ) value = Result $ liftStateM $ fn value
apply (LambdaV env arg body) value = pushFrame $ switchContext env $ do
  bindValue arg value
  eval' body
apply expr _ = fail $ "Expression '" ++ show expr ++ "' is not callable"

-- Pattern matching

patternMatch :: [PatternCase] -> Value -> Result (Maybe CoreExpr)
patternMatch []       _     = return Nothing
patternMatch (x : xs) value = do
  liftA2 (<|>) (tryMatch x value) (patternMatch xs value)
  where tryMatch x' = (<|> return Nothing) . patternMatch' x'

patternMatch' :: PatternCase -> Value -> Result (Maybe CoreExpr)
patternMatch' (DefaultP, result) _ = return $ Just result
-- Matching literals
patternMatch' (LitP (Int x), result) (LitV (Int y))
  | x == y    = return $ Just result
  | otherwise = return Nothing
patternMatch' (LitP (Bool x), result) (LitV (Bool y))
  | x == y    = return $ Just result
  | otherwise = return Nothing

-- Matching variables
patternMatch' (VarP   name, result) value = bindValue name value $> Just result

-- Matching tuples
patternMatch' (TupleP []  , result) value = apply value trueVal >>= matchTrue
  where
    trueVal   = LitV $ Bool True
    truePat   = LitP $ Bool True
    matchTrue = patternMatch' (truePat, result)

patternMatch' (TupleP (x : xs), result) value = do
  head' <- first tupleLength value
  tail' <- dropFirst tupleLength value
  patternMatch' (x, result) head' >>= continue tail'
  where
    tupleLength = length (x : xs)

    -- if the current pattern match failed, we short-circuit and return Nothing
    continue _    Nothing  = return Nothing
    -- if the current pattern match succeeded, we discard its partial result and
    -- pattern match the tail of the tuple
    continue rest (Just _) = patternMatch' (TupleP xs, result) rest

-- Unrecognized pattern
patternMatch' _ _ = return Nothing

lookupVariable :: Name -> Result Value
lookupVariable x = getEnv >>= unpack . (Map.!? x)
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

-- Tuple helpers

first :: Int -> Value -> Result Value
first n v = eval' (select' 1 n) >>= apply v

dropFirst :: Int -> Value -> Result Value
dropFirst n v = eval' (drop' 1 n) >>= apply v

select' :: Int -> Int -> CoreExpr
select' x n = mkLams args selector
  where
    args     = toArg <$> [1 .. n]
    selector = Var $ toArg x
    toArg    = ("$" ++) . show

drop' :: Int -> Int -> CoreExpr
drop' d n = foldr (Lam . toArg) apply' [1 .. n]
  where
    apply' = Lam "_" (Var "_" `mkApps` (Var . toArg <$> range))
    range | n == d    = []
          | otherwise = [(n - (d - 1)) .. n]
    toArg = ("$" ++) . show
