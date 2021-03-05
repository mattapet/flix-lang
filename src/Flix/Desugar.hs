{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}

module Flix.Desugar
  ( desugar
  ) where

import           Control.Applicative            ( liftA2 )
import           Core                    hiding ( Expr )
import           Data.Bifunctor                 ( first )
import           Data.Types
import           Flix.Capabilities
import           Flix.Desugar.Errors
import           Flix.Syntax

type Context m
  = ( MonadFail m
    , SymbolAliasRegistry m
    , UniqueNameGeneration m
    , ConstructorRegistry m
    )

desugar :: (Context m) => AST -> m CoreExpr
desugar (Expr e) = desugarExpr e
desugar (Decl e) = desugarDecl e

-- Declarations

desugarDecl :: (Context m) => Decl -> m CoreExpr
desugarDecl d = desugarDecl' d >>= desugarBlockExprs

desugarDecl' :: (Context m) => Decl -> m [Expr]
desugarDecl' (Module _ body) = concat <$> traverse go_body body
  where
    go_body (Decl d) = desugarDecl' d
    go_body (Expr e) = return [e]

desugarDecl' (Record constr fields) =
  bindConstructor constr go_constrTy' >> liftA2 (:) constructor accessors
  where
    constructor = do
      let args = Identifier <$> fields
      let body = Tuple args
      return $ Def constr [(args, body)]

    accessors = traverse generateAccessor fields
      where
        generateAccessor name = do
          arg <- generateUniqueName
          let body = Identifier arg `Call` [fieldExtractor name]
          return $ Def name [([Identifier arg], body)]
        fieldExtractor = Lambda fields . Identifier

    go_constrTy' = foldr (:~>) (NominalTy constr) (AnyTy <$ fields)

-- Expressions

desugarExpr :: (Context m) => Expr -> m CoreExpr
desugarExpr (BoolLiteral     x     ) = return $ Lit $ Bool x
desugarExpr (NumberLiteral   x     ) = return $ Lit $ Int x
desugarExpr (CharLiteral     x     ) = return $ Lit $ Char x
desugarExpr (Identifier      x     ) = return $ Var x
desugarExpr (Constructor     x     ) = return $ Var x
desugarExpr (OperatorCapture x     ) = return $ Var x
desugarExpr (StringLiteral xs) = desugarListLiteralExpr $ CharLiteral <$> xs
desugarExpr (ListLiteral     xs    ) = desugarListLiteralExpr xs
desugarExpr (Tuple           fields) = do
  nextId' <- generateUniqueName
  fields' <- traverse desugarExpr fields
  return $ Lam nextId' (mkApps (Var nextId') fields')

desugarExpr (Lambda []       body) = desugarExpr body
desugarExpr (Lambda [arg   ] body) = Lam arg <$> desugarExpr body
desugarExpr (Lambda (x : xs) body) = Lam x <$> desugarExpr (Lambda xs body)

desugarExpr (BinOp op x y        ) = desugarExpr $ Call (Identifier op) [x, y]

desugarExpr (Call callee args) =
  liftA2 mkApps (desugarExpr callee) (traverse desugarExpr args)

-- Maybe an error here???
desugarExpr (Def name cases) = do
  (args, body) <- simplifyDef cases
  body'        <- desugarExpr body
  let fn = foldr Lam body' args
  id' <- generateUniqueName
  return $ Lam id' (Bind (name, fn) (Var id'))

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
      liftA2 (,) (desugarCasePattern pattern) (desugarExpr result)

desugarExpr Underscore = undefined
desugarExpr Let{}      = undefined

simplifyDef :: (Context m) => [([Expr], Expr)] -> m ([Name], Expr)
simplifyDef cases@[(args, body)] = case collect_argNames args of
  -- Return simple def expression if arguments do not contain any patterns
  Just args' -> return (args', body)
  Nothing    -> translateToCaseExpr' cases
  where
    collect_argNames = traverse collect_argName
    collect_argName (Identifier x) = Just x
    collect_argName _              = Nothing
simplifyDef cases = translateToCaseExpr' cases

translateToCaseExpr' :: (Context m) => [([Expr], Expr)] -> m ([Name], Expr)
translateToCaseExpr' cases = all_same_length cases >> do
  args <- get_args
  let body' = Match (go_args args) (go_casePattern cases)
  return (args, body')

  where
    get_args = traverse (const generateUniqueName) [1 .. get_argLen cases]

    get_argLen []              = 0
    get_argLen ((args, _) : _) = length args

    go_args        = Tuple . fmap Identifier
    go_casePattern = fmap (first Tuple)

    all_same_length [] = return []
    all_same_length (x : xs)
      | all (\y -> length x == length y) xs = all_same_length xs
      | otherwise                           = fail defNumberOfArgumentsError

-- Helper functions

desugarListLiteralExpr :: (Context m) => [Expr] -> m CoreExpr
desugarListLiteralExpr xs = do
  (cons, nil) <- liftA2 (,) (lookupSymbolAlias ":") (lookupSymbolAlias "Nil")
  desugarExpr $ foldr (BinOp cons) (Identifier nil) xs

desugarBlockExprs :: (Context m) => [Expr] -> m CoreExpr
desugarBlockExprs es = do
  defs    <- traverse go_def $ foldMap go_collectDefs es
  content <- desugarBlockExprs' $ foldMap go_collectExprs es
  return $ foldr ($) content defs
  where
    go_def (name, cases) = do
      (args, body) <- simplifyDef cases
      body'        <- desugarExpr body
      return $ Bind (name, mkLams args body')

    go_collectDefs (Def name cases) = [(name, cases)]
    go_collectDefs _                = []

    go_collectExprs Def{} = []
    go_collectExprs e     = [e]

    desugarBlockExprs' []                               = fail emptyBlockError
    desugarBlockExprs' [Let{}] = fail letAtEndOfBlockError
    desugarBlockExprs' [x                             ] = desugarExpr x
    desugarBlockExprs' (Let (Identifier arg) body : xs) = do
      body'   <- desugarExpr body
      context <- desugarBlockExprs' xs
      return $ (arg, body') `Bind` context

    desugarBlockExprs' (Let arg body : xs) = do
      body'   <- desugarExpr body
      context <- desugarBlockExprs' xs
      arg'    <- desugarCasePattern arg
      return $ Case body' [(arg', context)]

    desugarBlockExprs' (x : xs) = do
      sideEffect  <- desugarExpr x
      context     <- desugarBlockExprs xs
      throwawayId <- generateUniqueName
      return $ (throwawayId, sideEffect) `Bind` context

desugarCasePattern :: (Context m) => Expr -> m Pattern
desugarCasePattern Underscore          = return DefaultP
desugarCasePattern (BoolLiteral   x  ) = return $ LitP $ Bool x
desugarCasePattern (NumberLiteral x  ) = return $ LitP $ Int x
desugarCasePattern (Identifier    x  ) = return $ VarP x
desugarCasePattern (Constructor   x  ) = flip ConstrP [] <$> lookupConstructor x
desugarCasePattern (Tuple es') = TupleP <$> traverse desugarCasePattern es'
desugarCasePattern (Call (Identifier constr) es') =
  liftA2 ConstrP (lookupConstructor constr) (traverse desugarCasePattern es')
desugarCasePattern (BinOp op lhs rhs) =
  liftA2 ConstrP (lookupConstructor op) (traverse desugarCasePattern [lhs, rhs])
desugarCasePattern e = fail $ unsupportedPatternMatchExprError e
