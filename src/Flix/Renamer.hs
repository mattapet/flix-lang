{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}

module Flix.Renamer
  ( rename
  ) where

import           Control.Applicative            ( liftA2
                                                , liftA3
                                                )
import           Control.Monad                  ( (>=>) )
import           Data.List                      ( intercalate
                                                , intersect
                                                )
import           Flix.Capabilities
import           Flix.Syntax

type Renaming m = (MonadFail m, ModuleRegistry m, SymbolAliasRegistry m)

-- Renaming

rename :: Renaming m => AST -> m AST
rename (Expr e) = Expr <$> renameExpr e
rename (Decl d) = Decl <$> renameDecl d

-- Expressions

renameExpr :: Renaming m => Expr -> m Expr
renameExpr Underscore           = return Underscore
renameExpr val@BoolLiteral{}    = return val
renameExpr val@NumberLiteral{}  = return val
renameExpr val@CharLiteral{}    = return val
renameExpr val@StringLiteral{}  = return val
renameExpr (OperatorCapture x ) = OperatorCapture <$> renameName x
renameExpr (ListLiteral     xs) = ListLiteral <$> traverse renameExpr xs
renameExpr (Tuple           xs) = Tuple <$> traverse renameExpr xs
renameExpr (Identifier      x ) = Identifier <$> renameName x
renameExpr (Constructor     x ) = Constructor <$> renameName x

renameExpr (BinOp op lhs rhs) =
  liftA3 BinOp (renameName op) (renameExpr lhs) (renameExpr rhs)

renameExpr (Call callee args) =
  liftA2 Call (renameExpr callee) (traverse renameExpr args)

renameExpr (Let arg body) = do
  body' <- renameExpr body
  _     <- introduceVariables $ foldMap go_collectVars [arg]
  arg'  <- renameExpr arg
  return $ Let arg' body'
  where
    go_collectVars (Identifier x) = [x]
    go_collectVars (Tuple      t) = foldMap go_collectVars t
    go_collectVars _              = []

renameExpr (Def name cases) =
  liftA2 Def (renameName name) (traverse renameCaseDef cases)

renameExpr (If cond then' else') =
  liftA3 If (renameExpr cond) (renameExpr then') (renameExpr else')

renameExpr (Lambda args body) =
  pushFrame $ liftA2 Lambda (introduceVariables args) (renameExpr body)

renameExpr (Block exprs) = pushFrame $ do
  _ <- introduceVariables $ foldMap go_collectDefName exprs
  Block <$> traverse renameExpr exprs
  where
    go_collectDefName (Def name _) = [name]
    go_collectDefName _            = []

renameExpr (Match value caseExprs) =
  liftA2 Match (renameExpr value) (traverse renameCaseExpr caseExprs)

renameName :: Renaming m => Name -> m Name
renameName = lookupSymbolAlias

renameCaseDef :: Renaming m => ([Expr], Expr) -> m ([Expr], Expr)
renameCaseDef (args, body) = pushFrame $ do
  _ <- introduceVariables $ foldMap collect_vars args
  liftA2 (,) (traverse renameExpr args) (renameExpr body)
  where
    collect_vars (Identifier x) = [x]
    collect_vars (Tuple      t) = foldMap collect_vars t
    collect_vars _              = []

renameCaseExpr :: Renaming m => CaseExpr -> m CaseExpr
renameCaseExpr (pattern, value) = pushFrame $ do
  _ <- introduceVariables $ collect_vars pattern
  liftA2 (,) (renameExpr pattern) (renameExpr value)
  where
    collect_vars (Identifier x) = [x]
    collect_vars (Tuple      t) = foldMap collect_vars t
    collect_vars _              = []

-- Declarations

renameDecl :: Renaming m => Decl -> m Decl
renameDecl (Module name contents) = do
  _ <- registerModule name
  _ <- introduceVariables $ foldMap go_collectDefNames contents
  Module name <$> traverse rename contents
  where
    go_collectDefNames (Expr (Def name' _)) = [name']
    go_collectDefNames _                    = []

renameDecl (Record name fields) =
  liftA2 Record (introduceVariable name) (introduceVariables fields)

-- Helper functions

introduceVariables :: Renaming m => [Name] -> m [Name]
introduceVariables = uniq >=> traverse registerSymbol

introduceVariable :: Renaming m => Name -> m Name
introduceVariable = registerSymbol

uniq :: Renaming m => [Name] -> m [Name]
uniq names = case collect_conflictingNames names [] of
  []        -> return names
  conflicts -> fail $ formatConflicts conflicts
  where
    collect_conflictingNames [] acc = acc
    collect_conflictingNames (x : xs) acc =
      collect_conflictingNames xs (acc ++ [x] `intersect` xs)

formatConflicts :: [Name] -> String
formatConflicts = wrapInMessage . sep . (quote <$>)
  where
    sep           = intercalate ", "
    quote         = ("'" ++) . (++ "'")
    wrapInMessage = ("Conflicting definition for symbols " ++)
