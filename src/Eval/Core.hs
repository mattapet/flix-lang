module Eval.Core where

import           Data.Maybe                     ( fromMaybe )

data CoreExpr =
    BoolE Bool
  | IntE Integer
  | VarE String
  | AppE CoreExpr CoreExpr
  | LamE String CoreExpr
  deriving (Eq, Show)

type Env = [(String, CoreExpr)]

eval :: Env -> CoreExpr -> CoreExpr
eval _   val@BoolE{}         = val
eval _   val@IntE{}          = val
eval env (VarE x)            = fromMaybe (VarE x) (lookup x env)
eval _   val@LamE{}          = val

eval env (AppE (LamE x b) e) = eval ((x, e) : env) b
eval _   val@AppE{}          = val
