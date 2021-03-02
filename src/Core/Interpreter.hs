{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.Interpreter
  ( eval
  , evalM
  ) where

import           Control.Applicative            ( (<|>)
                                                , Alternative(..)
                                                , liftA2
                                                )
import           Control.Monad.ExceptT
import           Control.Monad.Extra            ( bind2 )
import           Control.Monad.StateT
import           Core.Expr
import           Data.Functor                   ( ($>) )
import           Data.Functor.Identity
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Types

eval :: Environment -> Expr -> Either String (Value, Environment)
eval env = runIdentity . runExceptT . flip runStateT env . runResult . eval'

evalM :: (Monad m)
      => Environment
      -> Expr
      -> m (Either String (Value, Environment))
evalM env = runExceptT . flip runStateT env . runResult . eval'

newtype ResultT m a = ResultT { runResult :: StateT Environment (ExceptT String m) a }
  deriving (Monad, Applicative, Functor)

instance (Monad m) => MonadFail (ResultT m) where
  fail = ResultT . liftStateM . liftExceptT . Left

instance (Monad m) => Alternative (ResultT m) where
  empty = fail ""
  ResultT x <|> ResultT y =
    ResultT $ StateT $ \s -> runStateT x s <|> runStateT y s

getEnv :: (Monad m) => ResultT m Environment
getEnv = ResultT get

setEnv :: (Monad m) => Environment -> ResultT m ()
setEnv = ResultT . put

getScope :: (Monad m) => ResultT m Scope
getScope = env_scope <$> getEnv

setScope :: (Monad m) => Scope -> ResultT m ()
setScope s = getEnv >>= setEnv . Environment s . env_constr

getConstructors :: (Monad m) => ResultT m Constructors
getConstructors = env_constr <$> getEnv

-- Evaluation

eval' :: (Monad m) => Expr -> ResultT m Value
eval' (Lit x       ) = return $ LitV x
eval' (Var x       ) = liftA2 bindType (lookupType x) (lookupVariable x)
eval' (Lam arg body) = do
  scope <- getScope
  return $ LambdaV AnyTy scope arg body

eval' (App  callee        arg    ) = bind2 apply (eval' callee) (eval' arg)

eval' (Bind (name, value) context) = pushFrame $ do
  eval' value >>= bindValue name
  eval' context

eval' (Case value patterns) = pushFrame $ do
  value' <- eval' value
  result <- patternMatch patterns value'
  maybe (fail $ "Pattern match fallthrough on value " ++ show value')
        eval'
        result

-- Lambda applications

apply :: (Monad m) => Value -> Value -> ResultT m Value
apply (BuiltinV _ fn) value = ResultT $ liftStateM $ liftExceptT $ fn value
apply (LambdaV ty scope arg body) value = pushFrame $ switchContext scope $ do
  bindValue arg value
  applyType ty <$> eval' body
apply expr _ = fail $ "Expression '" ++ show expr ++ "' is not callable"

applyType :: Ty -> Value -> Value
applyType (_ :~> ty) (LambdaV _ s n c) = LambdaV ty s n c
applyType _          v                 = v

-- Pattern matching

patternMatch :: (Monad m) => [PatternCase] -> Value -> ResultT m (Maybe Expr)
patternMatch []       _     = return Nothing
patternMatch (x : xs) value = do
  liftA2 (<|>) (tryMatch x value) (patternMatch xs value)
  where tryMatch x' = (<|> return Nothing) . patternMatch' x'

patternMatch' :: (Monad m) => PatternCase -> Value -> ResultT m (Maybe Expr)
patternMatch' (DefaultP, result) _ = return $ Just result
-- Matching literals
patternMatch' (LitP x, result) (LitV y) | x == y    = return $ Just result
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

patternMatch' (ConstrP ty ptrns, result) value@(LambdaV ty' _ _ _) =
  go_continue $ go_unpackTys ty ty'
  where
    go_unpackTys (_ :~> lhs    ) rhs             = go_unpackTys lhs rhs
    go_unpackTys (NominalTy lhs) (NominalTy rhs) = Just (lhs, rhs)
    go_unpackTys _               _               = Nothing

    go_continue (Just (lhs, rhs))
      | lhs == rhs = patternMatch' (TupleP ptrns, result) value
      | otherwise  = return Nothing
    go_continue Nothing = return Nothing

-- Unrecognized pattern
patternMatch' _ _ = return Nothing

lookupVariable :: (Monad m) => Name -> ResultT m Value
lookupVariable x = getScope >>= unpack . (Map.!? x)
  where
    unpack (Just v) = return v
    unpack Nothing  = fail $ "Unbound variable '" ++ x ++ "'"

lookupType :: (Monad m) => Name -> ResultT m Ty
lookupType x = fromMaybe AnyTy . (Map.!? x) <$> getConstructors

bindValue :: (Monad m) => Name -> Value -> ResultT m ()
bindValue name value = getScope >>= setScope . Map.insert name value

bindType :: Ty -> Value -> Value
bindType ty (LambdaV AnyTy s n c) = LambdaV ty s n c
bindType _  v                     = v

pushFrame :: (Monad m) => ResultT m a -> ResultT m a
pushFrame f = do
  env    <- getEnv
  result <- f
  setEnv env $> result

switchContext :: (Monad m) => Scope -> ResultT m a -> ResultT m a
switchContext env f = pushFrame $ do
  baseScope <- getScope
  setScope (env <> baseScope) >> f

-- Tuple helpers

first :: (Monad m) => Int -> Value -> ResultT m Value
first n v = eval' (select' 1 n) >>= apply v

dropFirst :: (Monad m) => Int -> Value -> ResultT m Value
dropFirst n v = eval' (drop' 1 n) >>= apply v

select' :: Int -> Int -> Expr
select' x n = mkLams args selector
  where
    args     = toArg <$> [1 .. n]
    selector = Var $ toArg x
    toArg    = ("$" ++) . show

drop' :: Int -> Int -> Expr
drop' d n = foldr (Lam . toArg) apply' [1 .. n]
  where
    apply' = Lam "_" (Var "_" `mkApps` (Var . toArg <$> [(d + 1) .. n]))
    toArg  = ("$" ++) . show
