module Syntax.Core where

type Name = String
type OperatorName = String

type CaseExpr = (Expr, Expr)

data Expr =
    Underscore
  | BoolLiteral Bool
  | NumberLiteral Integer
  | Identifier String
  | Call Expr [Expr]
  | BinOp OperatorName Expr Expr
  | Let Name [Name] Expr
  | If Expr Expr Expr
  | Block [Expr]
  | Match Expr [CaseExpr]
  deriving (Show, Eq)

data AST = Expr Expr
  deriving (Show, Eq)

