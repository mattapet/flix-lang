{-# LANGUAGE TupleSections #-}

module Syntax.Desugar
  ( desugar
  ) where

import           Control.Applicative            ( liftA2 )
import           Control.Monad.StateT
import           Data.Foldable                  ( foldlM )
import           Data.Functor                   ( ($>) )
import qualified Eval.Core                     as C
import qualified Syntax.Core                   as S

desugar :: S.AST -> Either String C.CoreExpr
desugar ast = fst <$> runStateT (desugar' ast) 0

type Result a = StateT Int (Either String) a

desugar' :: S.AST -> Result C.CoreExpr
desugar' (S.Expr e) = desugarExpr e
desugar' (S.Decl e) = desugarDecl e

-- Expressions

desugarExpr :: S.Expr -> Result C.CoreExpr
desugarExpr (S.BoolLiteral     x     ) = return $ C.Lit $ C.Bool x
desugarExpr (S.NumberLiteral   x     ) = return $ C.Lit $ C.Int x
desugarExpr (S.Identifier      x     ) = return $ C.Var x
desugarExpr (S.OperatorCapture x     ) = return $ C.Var x
desugarExpr (S.Tuple           fields) = do
  nextId' <- nextId
  fields' <- traverse desugarExpr fields
  return $ C.Lam nextId' (foldl C.App (C.Var nextId') fields')
desugarExpr (S.Lambda [arg] body) = C.Lam arg <$> desugarExpr body
desugarExpr (S.Lambda (x : xs) body) =
  C.Lam x <$> desugarExpr (S.Lambda xs body)

desugarExpr (S.BinOp op x y) = desugarExpr $ S.Call (S.Identifier op) [x, y]

desugarExpr (S.Call x []) = desugarExpr x
desugarExpr (S.Call callee (x : xs)) = inner >>= mkApps xs
  where
    inner  = C.App <$> desugarExpr callee <*> desugarExpr x
    mkApps = flip (foldlM mkApp)
    mkApp callee' arg = C.App callee' <$> desugarExpr arg

-- Maybe an error here???
desugarExpr (S.Let n args body) = do
  body' <- desugarExpr body
  let fn = foldr C.Lam body' args
  id' <- nextId
  return $ C.Lam id' (C.Bind (n, fn) (C.Var id'))

desugarExpr (S.Block xs           ) = desugarBlockExprs xs


desugarExpr (S.If cond then' else') = do
  liftA2 C.Case (desugarExpr cond) conditionalPattern
  where
    then''             = (C.LitP (C.Bool True), ) <$> desugarExpr then'
    else''             = (C.LitP (C.Bool False), ) <$> desugarExpr else'
    conditionalPattern = sequence [then'', else'']

desugarExpr (S.Match value cases) = do
  value' <- desugarExpr value
  cases' <- traverse desugarCase cases
  return $ C.Case value' cases'
  where
    desugarCase (pattern, result) =
      liftA2 (,) (desugarPattern pattern) (desugarExpr result)
    desugarPattern S.Underscore         = return C.DefaultP
    desugarPattern (S.BoolLiteral   x ) = return $ C.LitP $ C.Bool x
    desugarPattern (S.NumberLiteral x ) = return $ C.LitP $ C.Int x
    desugarPattern (S.Identifier    x ) = return $ C.VarP x
    desugarPattern (S.Tuple es) = C.TupleP <$> traverse desugarPattern es
    desugarPattern _                    = fail "Unsupported pattern match expr"


desugarExpr _ = undefined


-- Declarations

desugarDecl :: S.Decl -> Result C.CoreExpr
desugarDecl d = desugarDecl' d >>= desugarBlockExprs

desugarDecl' :: S.Decl -> Result [S.Expr]
desugarDecl' (S.Module _ body) = concat <$> traverse transformToExpr' body
  where
    transformToExpr' (S.Decl d) = desugarDecl' d
    transformToExpr' (S.Expr e) = return [e]

desugarDecl' (S.Record constr fields) = (constructor :) <$> accessors
  where
    constructor = S.Let constr fields (S.Tuple (S.Identifier <$> fields))
    accessors   = traverse generateAccessor fields
    generateAccessor name = do
      id' <- nextId
      return $ S.Let
        name
        [id']
        (S.Call (S.Identifier id') [S.Lambda fields (S.Identifier name)])

-- Helper functions

nextId :: Result C.Name
nextId = do
  nextId' <- (+ 1) <$> get
  put nextId' $> "_$" ++ show nextId'

desugarBlockExprs :: [S.Expr] -> Result C.CoreExpr
desugarBlockExprs []                          = fail "Unexpected empty block"
desugarBlockExprs [S.Let{}] = fail "Illegal binding at the end of the block"
desugarBlockExprs [x                        ] = desugarExpr x
desugarBlockExprs (S.Let name args body : xs) = do
  body' <- desugarExpr body
  let fn = foldr C.Lam body' args
  context <- desugarBlockExprs xs
  return $ C.Bind (name, fn) context
-- Wrap side-effect-y expression withing a noop lambda application that ignores
-- its argument
desugarBlockExprs (x : xs) = do
  sideEffect  <- desugarExpr x
  context     <- desugarBlockExprs xs
  throwawayId <- nextId
  return $ C.Bind (throwawayId, sideEffect) context
