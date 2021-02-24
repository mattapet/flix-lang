module Syntax.Core where

type Name = String
type OperatorName = String

data AST =
    BoolLiteral Bool
  | NumberLiteral Integer
  | Identifier String
  | Call AST [AST]
  | BinOp OperatorName AST AST
  | Let Name [Name] AST
  | If AST AST AST
  | Block [AST]
  deriving (Show, Eq)

