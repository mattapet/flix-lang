module Eval.Core where

import           Data.Map                       ( Map )

infix 4 `Bind`, `mkApps`, `App`

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

type Scope = Map Name Value
type Constructors = Map Name Ty

data Environment = Environment
  { env_scope  :: Scope
  , env_constr :: Constructors
  }

type Builtin = Value -> Either String Value

data Value =
    LitV Literal
  | LambdaV Scope Name CoreExpr
  | BuiltinV Name Builtin

instance Show Value where
  show (LitV lit       ) = "LitV " ++ show lit
  show (LambdaV _ arg e) = "<λ: " ++ " " ++ show arg ++ " " ++ show e ++ ">"
  show (BuiltinV name _) = "<builtin " ++ name ++ ">"


-- Types

data Ty =
    AnyTy
  | NominalTy String
  | Ty :~> Ty
  deriving (Show, Eq)

-- Helper building functions

mkApps :: CoreExpr -> [Arg] -> CoreExpr
mkApps = foldl App

mkLams :: [Name] -> CoreExpr -> CoreExpr
mkLams args body = foldr Lam body args

mkBinds :: [Bind] -> CoreExpr -> CoreExpr
mkBinds binds body = foldr Bind body binds
