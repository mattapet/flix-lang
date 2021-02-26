module Syntax.Desugar where

import           Control.Monad.StateT
import           Data.Foldable                  ( foldlM )
import           Data.Functor                   ( ($>) )
import qualified Eval.Core                     as C
import qualified Syntax.Core                   as S

desugar :: S.AST -> Either String C.CoreExpr
desugar (S.Expr e) = fst <$> runStateT (desugarE e) 0

type Result a = StateT Int (Either String) a

desugarE :: S.Expr -> Result C.CoreExpr
desugarE (S.BoolLiteral   x     ) = return $ C.Lit $ C.Bool x
desugarE (S.NumberLiteral x     ) = return $ C.Lit $ C.Int x
desugarE (S.Identifier    x     ) = return $ C.Var x
desugarE (S.Lambda [arg   ] body) = C.Lam arg <$> desugarE body
desugarE (S.Lambda (x : xs) body) = C.Lam x <$> desugarE (S.Lambda xs body)

desugarE (S.BinOp op x y        ) = desugarE $ S.Call (S.Identifier op) [x, y]
desugarE (S.Call x      []      ) = desugarE x
desugarE (S.Call callee (x : xs)) = inner >>= flip (foldlM wrapApp) xs
  where
    inner = C.App <$> desugarE callee <*> desugarE x
    wrapApp callee' arg = C.App callee' <$> desugarE arg

-- Maybe an error here???
desugarE (S.Let n args body) = do
  body' <- desugarE body
  let fn = foldr C.Lam body' args
  id' <- nextId
  return $ C.Lam id' (C.Bind (n, fn) (C.Var id'))

desugarE (S.Block xs           ) = desugarBlockExprs xs


desugarE (S.If cond then' else') = do
  cond'  <- desugarE cond
  then'' <- desugarE then'
  else'' <- desugarE else'
  return
    $ C.Case cond' [C.LitP (C.Bool True) then'', C.LitP (C.Bool False) else'']

desugarE (S.Match value cases) = do
  value' <- desugarE value
  cases' <- traverse desugarCase cases
  return $ C.Case value' cases'
  where
    desugarCase (S.BoolLiteral   x, expr) = C.LitP (C.Bool x) <$> desugarE expr
    desugarCase (S.NumberLiteral x, expr) = C.LitP (C.Int x) <$> desugarE expr
    desugarCase (S.Underscore     , expr) = C.DefaultP <$> desugarE expr


desugarE _ = undefined

-- Helper functions

nextId :: Result C.Name
nextId = do
  nextId' <- (+ 1) <$> get
  put nextId' $> "_$" ++ show nextId'

desugarBlockExprs :: [S.Expr] -> Result C.CoreExpr
desugarBlockExprs []                          = fail "Unexpected empty block"
desugarBlockExprs [S.Let{}] = fail "Illegal binding at the end of the block"
desugarBlockExprs [x                        ] = desugarE x
desugarBlockExprs (S.Let name args body : xs) = do
  body' <- desugarE body
  let fn = foldr C.Lam body' args
  context <- desugarBlockExprs xs
  return $ C.Bind (name, fn) context
-- Wrap side-effect-y expression withing a noop lambda application that ignores
-- its argument
desugarBlockExprs (x : xs) = do
  sideEffect  <- desugarE x
  context     <- desugarBlockExprs xs
  throwawayId <- nextId
  return $ C.Bind (throwawayId, sideEffect) context
