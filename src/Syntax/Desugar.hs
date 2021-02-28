{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Syntax.Desugar
  ( desugar
  ) where

import           Control.Applicative            ( liftA2 )
import           Control.Monad.StateT
import           Data.Bifunctor                 ( first )
import           Data.Functor                   ( ($>) )
import qualified Data.Map                      as Map
import qualified Eval.Core                     as C
import qualified Syntax.Core                   as S

desugar :: S.AST -> Either String (C.CoreExpr, C.Constructors)
desugar ast = unwrap <$> runStateT (desugar' ast) makeEmptyState
  where unwrap = (state_constructors <$>)

data DesugarState = DesugarState
  { state_uniqueNameCounter :: Int
  , state_constructors      :: C.Constructors
  }

makeEmptyState :: DesugarState
makeEmptyState = DesugarState 0 mempty

getState :: Result DesugarState
getState = get

setState :: DesugarState -> Result ()
setState = put

getNextId :: Result Int
getNextId = do
  id'    <- (+ 1) . state_uniqueNameCounter <$> getState
  constr <- state_constructors <$> getState
  setState (DesugarState id' constr) $> id'

setConstructors :: C.Constructors -> Result ()
setConstructors constr = do
  uniques <- state_uniqueNameCounter <$> getState
  setState (DesugarState uniques constr)

bindConstructor :: C.Name -> C.Ty -> Result ()
bindConstructor n ty = do
  constrs <- state_constructors <$> getState
  setConstructors $ Map.insert n ty constrs


type Result a = StateT DesugarState (Either String) a

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

desugarExpr (S.Call callee args) =
  liftA2 C.mkApps (desugarExpr callee) (traverse desugarExpr args)

-- Maybe an error here???
desugarExpr (S.Let n args body) = do
  body' <- desugarExpr body
  let fn = foldr C.Lam body' args
  id' <- nextId
  return $ C.Lam id' (C.Bind (n, fn) (C.Var id'))

desugarExpr (S.LetMatch n cases   ) = simplifyLetMatch n cases >>= desugarExpr
desugarExpr (S.Block xs           ) = desugarBlockExprs xs

desugarExpr (S.If cond then' else') = do
  liftA2 C.Case (desugarExpr cond) conditionalPattern
  where
    then''             = (C.LitP (C.Bool True), ) <$> desugarExpr then'
    else''             = (C.LitP (C.Bool False), ) <$> desugarExpr else'
    conditionalPattern = sequence [then'', else'']

desugarExpr (S.Match value cases) = do
  liftA2 C.Case (desugarExpr value) (traverse desugarCaseExpr cases)
  where
    desugarCaseExpr (pattern, result) =
      liftA2 (,) (go_casePattern pattern) (desugarExpr result)

    go_casePattern S.Underscore         = return C.DefaultP
    go_casePattern (S.BoolLiteral   x ) = return $ C.LitP $ C.Bool x
    go_casePattern (S.NumberLiteral x ) = return $ C.LitP $ C.Int x
    go_casePattern (S.Identifier    x ) = return $ C.VarP x
    go_casePattern (S.Tuple es) = C.TupleP <$> traverse go_casePattern es
    go_casePattern e =
      fail $ "Unsupported pattern match expr '" ++ show e ++ "'"


desugarExpr _ = undefined

simplifyLetMatch :: S.Name -> [([S.Expr], S.Expr)] -> Result S.Expr
simplifyLetMatch n cases@[(args, body)] = case collect_argNames args of
  -- Return simple let expression if arguments do not contain any patterns
  Just args' -> return $ S.Let n args' body
  Nothing    -> translateToCaseExpr n cases
  where
    collect_argNames = traverse collect_argName
    collect_argName (S.Identifier x) = Just x
    collect_argName _                = Nothing
simplifyLetMatch n cases = translateToCaseExpr n cases

translateToCaseExpr :: S.Name -> [([S.Expr], S.Expr)] -> Result S.Expr
translateToCaseExpr n cases = all_same_length cases >> do
  args <- get_args
  let body' = S.Match (go_args args) (go_casePatterns cases)
  return $ S.Let n args body'

  where
    get_args = traverse (const nextId) [1 .. get_argLen cases]

    get_argLen []              = 0
    get_argLen ((args, _) : _) = length args

    go_args         = S.Tuple . fmap S.Identifier
    go_casePatterns = fmap (first S.Tuple)

    all_same_length [] = return []
    all_same_length (x : xs)
      | all (\y -> length x == length y) xs = all_same_length xs
      | otherwise = fail "Let bindings with different amount of arguments"

-- Declarations

desugarDecl :: S.Decl -> Result C.CoreExpr
desugarDecl d = desugarDecl' d >>= desugarBlockExprs

desugarDecl' :: S.Decl -> Result [S.Expr]
desugarDecl' (S.Module _ body) = concat <$> traverse go_body body
  where
    go_body (S.Decl d) = desugarDecl' d
    go_body (S.Expr e) = return [e]

desugarDecl' (S.Record constr fields) =
  bindConstructor constr constrTy >> (constructor :) <$> accessors
  where
    constructor = S.Let constr fields (S.Tuple (S.Identifier <$> fields))
    accessors   = traverse generateAccessor fields
    generateAccessor name = do
      arg <- nextId
      return
        $ S.Let name [arg] (S.Identifier arg `S.Call` [fieldExtractor name])
    fieldExtractor = S.Lambda fields . S.Identifier
    constrTy       = foldr (C.:~>) (C.NominalTy constr) (C.AnyTy <$ fields)

-- Helper functions

nextId :: Result C.Name
nextId = ("_$" ++) . show <$> getNextId

desugarBlockExprs :: [S.Expr] -> Result C.CoreExpr
desugarBlockExprs []                          = fail "Unexpected empty block"
desugarBlockExprs [S.Let{}] = fail "Illegal binding at the end of the block"
desugarBlockExprs [x                        ] = desugarExpr x
desugarBlockExprs (S.Let name args body : xs) = do
  fn      <- C.mkLams args <$> desugarExpr body
  context <- desugarBlockExprs xs
  return $ (name, fn) `C.Bind` context

desugarBlockExprs (S.LetMatch n cs : xs) =
  simplifyLetMatch n cs >>= desugarBlockExprs . (: xs)

-- Wrap side-effect-y expression withing a noop lambda application that ignores
-- its argument
desugarBlockExprs (x : xs) = do
  sideEffect  <- desugarExpr x
  context     <- desugarBlockExprs xs
  throwawayId <- nextId
  return $ (throwawayId, sideEffect) `C.Bind` context
