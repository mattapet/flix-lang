module Core.Expr where

import           Data.Map                       ( Map )
import           Data.Types

infix 4 `Bind`, `mkApps`, `App`
infixr :~>

-- Core expressions

type CoreExpr = Expr

data Expr =
    Lit Literal
  | Var Name
  | App Expr Arg
  | Bind Bind Body
  | Lam Name Body
  | Case Expr [PatternCase]
  deriving (Show, Eq)

type Name = String
type Arg = Expr
type Body = Expr
type Bind = (Name, Arg)
type PatternCase = (Pattern, Expr)

data Pattern =
    LitP Literal
  | VarP Name
  | TupleP [Pattern]
  | ConstrP Ty [Pattern]
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
  | LambdaV Ty Scope Name Expr
  | BuiltinV Name Builtin

instance Show Value where
  show (LitV lit) = "LitV " ++ show lit
  show (LambdaV ty _ arg e) =
    "<λ: " ++ show ty ++ " " ++ show arg ++ " " ++ show e ++ ">"
  show (BuiltinV name _) = "<builtin " ++ name ++ ">"


-- Types

data Ty =
    AnyTy
  | NominalTy String
  | Ty :~> Ty
  deriving (Show, Eq)

-- Helper building functions

mkApps :: Expr -> [Arg] -> Expr
mkApps = foldl App

mkLams :: [Name] -> Expr -> Expr
mkLams args body = foldr Lam body args

mkBinds :: [Bind] -> Expr -> Expr
mkBinds binds body = foldr Bind body binds
