module Flix.Syntax where

type Name = String
type OpName = String
type FieldName = String

type CaseExpr = (Expr, Expr)

type Arg = Expr
type Args = [Arg]
type Body = Expr

data Expr =
    Underscore
  | BoolLiteral Bool
  | NumberLiteral Integer
  | CharLiteral Char
  | StringLiteral String
  | ListLiteral [Expr]
  | Identifier Name
  | Constructor String
  | Call Expr [Expr]
  | BinOp OpName Expr Expr
  | OperatorCapture OpName
  | Tuple [Expr]
  | Let Arg Expr
  | Def Name [(Args, Body)]
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

