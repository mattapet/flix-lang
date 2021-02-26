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
  | Case CoreExpr [Pattern]
  deriving (Show, Eq)

type Name = String
type Arg = CoreExpr
type Body = CoreExpr
type Bind = (Name, Arg)

data Pattern =
    LitP Literal CoreExpr
  | DefaultP CoreExpr
  deriving (Show, Eq)

data CaseResult =
    Match Value
  | Fail
  deriving Show

instance Semigroup CaseResult where
  val@Match{} <> _ = val
  Fail        <> x = x

instance Monoid CaseResult where
  mempty = Fail

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
