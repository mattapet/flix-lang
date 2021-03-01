module Flix.Syntax where

type Name = String
type OpName = String
type FieldName = String

type CaseExpr = (Expr, Expr)

data Expr =
    Underscore
  | BoolLiteral Bool
  | NumberLiteral Integer
  | CharLiteral Char
  | Identifier String
  | Constructor String
  | Call Expr [Expr]
  | BinOp OpName Expr Expr
  | OperatorCapture OpName
  | Tuple [Expr]
  | Let Name [Name] Expr
  | LetMatch Name [([Expr], Expr)]
  | If Expr Expr Expr
  | Block [Expr]
  | Match Expr [CaseExpr]
  | Lambda [Name] Expr
  deriving (Show, Eq)

data Decl =
    Module Name [AST]
  | Record Name [FieldName]
  deriving (Show, Eq)

data AST =
    Expr Expr
  | Decl Decl
  deriving (Show, Eq)

