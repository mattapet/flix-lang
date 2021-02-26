module Eval.Core where

import           Data.Map                       ( Map )

data Literal =
    Bool Bool
  | Int Integer
  deriving (Show, Eq)

-- Core expressions

data CoreExpr =
    Lit Literal
  | Var Name
  | App CoreExpr Arg
  | Bind Bind Body
  | Lam Name Body
  | Case CoreExpr [PatternCase]
  deriving (Show, Eq)

type Name = String
type Arg = CoreExpr
type Body = CoreExpr
type Bind = (Name, Arg)
type PatternCase = (Pattern, CoreExpr)

data Pattern =
    LitP Literal
  | VarP Name
  | TupleP [Pattern]
  | DefaultP
  deriving (Show, Eq)

-- Values

type Environment = Map Name Value

type Builtin = Value -> Either String Value

data Value =
    LitV Literal
  | LambdaV Environment Name CoreExpr
  | BuiltinV Name Builtin

instance Show Value where
  show (LitV lit       ) = "LitV " ++ show lit
  show (LambdaV _ arg e) = "LambdaV <env> " ++ show arg ++ " " ++ show e
  show (BuiltinV name _) = "<builtin " ++ name ++ ">"
