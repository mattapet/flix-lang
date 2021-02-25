module Eval.Core where

data Literal =
    Bool Bool
  | Int Integer
  deriving (Show, Eq)

data CoreExpr =
    Lit Literal
  | Var Name
  | Let Bind CoreExpr
  | App CoreExpr Arg
  | Lam Name CoreExpr
  | Case CoreExpr [Pattern]
  deriving (Show, Eq)

type Name = String
type Arg = CoreExpr
type Bind = (Name, CoreExpr)

data Pattern =
    LitP Literal CoreExpr
  | Default CoreExpr
  | ErrorP
  deriving (Show, Eq)
