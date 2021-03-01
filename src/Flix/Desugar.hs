{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}

module Flix.Desugar
  ( desugar
  , makeEmptyState
  , DesugarState(..)
  ) where

import           Control.Applicative            ( liftA2 )
import           Control.Monad.StateT
import           Core                    hiding ( Expr )
import           Data.Bifunctor                 ( first )
import           Data.Functor                   ( ($>) )
import qualified Data.Map                      as Map
import           Data.Types
import           Flix.Syntax

desugar :: DesugarState -> AST -> Either String (CoreExpr, Constructors)
desugar state ast = unwrap <$> runStateT (desugar' ast) state
  where unwrap = (state_constructors <$>)

data DesugarState = DesugarState
  { state_uniqueNameCounter :: Int
  , state_constructors      :: Constructors
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

setConstructors :: Constructors -> Result ()
setConstructors constr = do
  uniques <- state_uniqueNameCounter <$> getState
  setState (DesugarState uniques constr)

bindConstructor :: Name -> Ty -> Result ()
bindConstructor n ty = do
  constrs <- state_constructors <$> getState
  setConstructors $ Map.insert n ty constrs

lookupConstr :: Name -> Result Ty
lookupConstr n = getState >>= unpack . (Map.!? n) . state_constructors
  where
    unpack (Just ty) = return ty
    unpack Nothing   = fail $ "Constructor '" ++ n ++ "' not found"

type Result a = StateT DesugarState (Either String) a

desugar' :: AST -> Result CoreExpr
desugar' (Expr e) = desugarExpr e
desugar' (Decl e) = desugarDecl e

-- Expressions

desugarExpr :: Expr -> Result CoreExpr
desugarExpr (BoolLiteral     x     ) = return $ Lit $ Bool x
desugarExpr (NumberLiteral   x     ) = return $ Lit $ Int x
desugarExpr (CharLiteral     x     ) = return $ Lit $ Char x
desugarExpr (Identifier      x     ) = return $ Var x
desugarExpr (Constructor     x     ) = return $ Var x
desugarExpr (OperatorCapture x     ) = return $ Var x
desugarExpr (Tuple           fields) = do
  nextId' <- nextId
  fields' <- traverse desugarExpr fields
  return $ Lam nextId' (foldl App (Var nextId') fields')

desugarExpr (Lambda [arg   ] body) = Lam arg <$> desugarExpr body
desugarExpr (Lambda (x : xs) body) = Lam x <$> desugarExpr (Lambda xs body)

desugarExpr (BinOp op x y        ) = desugarExpr $ Call (Identifier op) [x, y]

desugarExpr (Call callee args) =
  liftA2 mkApps (desugarExpr callee) (traverse desugarExpr args)

-- Maybe an error here???
desugarExpr (Let n args body) = do
  body' <- desugarExpr body
  let fn = foldr Lam body' args
  id' <- nextId
  return $ Lam id' (Bind (n, fn) (Var id'))

desugarExpr (LetMatch n cases   ) = simplifyLetMatch n cases >>= desugarExpr
desugarExpr (Block xs           ) = desugarBlockExprs xs

desugarExpr (If cond then' else') = do
  liftA2 Case (desugarExpr cond) conditionalPattern
  where
    then''             = (LitP (Bool True), ) <$> desugarExpr then'
    else''             = (LitP (Bool False), ) <$> desugarExpr else'
    conditionalPattern = sequence [then'', else'']

desugarExpr (Match value cases) = do
  liftA2 Case (desugarExpr value) (traverse desugarCaseExpr cases)
  where
    desugarCaseExpr (pattern, result) =
      liftA2 (,) (go_casePattern pattern) (desugarExpr result)

    go_casePattern Underscore         = return DefaultP
    go_casePattern (BoolLiteral   x ) = return $ LitP $ Bool x
    go_casePattern (NumberLiteral x ) = return $ LitP $ Int x
    go_casePattern (Identifier    x ) = return $ VarP x
    go_casePattern (Constructor   x ) = flip ConstrP [] <$> go_constrTy x
    go_casePattern (Tuple         es) = TupleP <$> traverse go_casePattern es
    go_casePattern (Call (Identifier constr) es) =
      liftA2 ConstrP (go_constrTy constr) (traverse go_casePattern es)
    go_casePattern (BinOp op lhs rhs) =
      liftA2 ConstrP (go_constrTy op) (traverse go_casePattern [lhs, rhs])
    go_casePattern e =
      fail $ "Unsupported pattern match expr '" ++ show e ++ "'"

    go_constrTy = lookupConstr


desugarExpr _ = undefined

simplifyLetMatch :: Name -> [([Expr], Expr)] -> Result Expr
simplifyLetMatch n cases@[(args, body)] = case collect_argNames args of
  -- Return simple let expression if arguments do not contain any patterns
  Just args' -> return $ Let n args' body
  Nothing    -> translateToCaseExpr n cases
  where
    collect_argNames = traverse collect_argName
    collect_argName (Identifier x) = Just x
    collect_argName _              = Nothing
simplifyLetMatch n cases = translateToCaseExpr n cases

translateToCaseExpr :: Name -> [([Expr], Expr)] -> Result Expr
translateToCaseExpr n cases = all_same_length cases >> do
  args <- get_args
  let body' = Match (go_args args) (go_casePatterns cases)
  return $ Let n args body'

  where
    get_args = traverse (const nextId) [1 .. get_argLen cases]

    get_argLen []              = 0
    get_argLen ((args, _) : _) = length args

    go_args         = Tuple . fmap Identifier
    go_casePatterns = fmap (first Tuple)

    all_same_length [] = return []
    all_same_length (x : xs)
      | all (\y -> length x == length y) xs = all_same_length xs
      | otherwise = fail "Let bindings with different amount of arguments"

-- Declarations

desugarDecl :: Decl -> Result CoreExpr
desugarDecl d = desugarDecl' d >>= desugarBlockExprs

desugarDecl' :: Decl -> Result [Expr]
desugarDecl' (Module _ body) = concat <$> traverse go_body body
  where
    go_body (Decl d) = desugarDecl' d
    go_body (Expr e) = return [e]

desugarDecl' (Record constr fields) =
  bindConstructor constr constrTy >> (constructor :) <$> accessors
  where
    constructor = Let constr fields (Tuple (Identifier <$> fields))
    accessors   = traverse generateAccessor fields
    generateAccessor name = do
      arg <- nextId
      return $ Let name [arg] (Identifier arg `Call` [fieldExtractor name])
    fieldExtractor = Lambda fields . Identifier
    constrTy       = foldr (:~>) (NominalTy constr) (AnyTy <$ fields)

-- Helper functions

nextId :: Result Name
nextId = ("_$" ++) . show <$> getNextId

desugarBlockExprs :: [Expr] -> Result CoreExpr
desugarBlockExprs []                        = fail "Unexpected empty block"
desugarBlockExprs [Let{}] = fail "Illegal binding at the end of the block"
desugarBlockExprs [x                      ] = desugarExpr x
desugarBlockExprs (Let name args body : xs) = do
  fn      <- mkLams args <$> desugarExpr body
  context <- desugarBlockExprs xs
  return $ (name, fn) `Bind` context

desugarBlockExprs (LetMatch n cs : xs) =
  simplifyLetMatch n cs >>= desugarBlockExprs . (: xs)

-- Wrap side-effect-y expression withing a noop lambda application that ignores
-- its argument
desugarBlockExprs (x : xs) = do
  sideEffect  <- desugarExpr x
  context     <- desugarBlockExprs xs
  throwawayId <- nextId
  return $ (throwawayId, sideEffect) `Bind` context
