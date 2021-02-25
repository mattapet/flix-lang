module Syntax.Core where

type Name = String
type OperatorName = String

data Expr =
    BoolLiteral Bool
  | NumberLiteral Integer
  | Identifier String
  | Call Expr [Expr]
  | BinOp OperatorName Expr Expr
  | Let Name [Name] Expr
  | If Expr Expr Expr
  | Block [Expr]
  deriving (Show, Eq)

data AST = Expr Expr
  deriving (Show, Eq)

