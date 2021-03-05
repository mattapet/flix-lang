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
rename (Decl d) = Decl <$> renameDecl d
rename (Expr e) = Expr <$> renameExpr e

-- Declarations

renameDecl :: Renaming m => Decl -> m Decl
renameDecl (Module name contents) = do
  _ <- registerModule name
  _ <- introduceVariables $ foldMap go_collectDefNames contents
  Module name <$> traverse rename contents
  where
    go_collectDefNames (Expr (Def    name' _     )) = [name']
    go_collectDefNames (Decl (Record name' fields)) = name' : fields
    go_collectDefNames _                            = []

renameDecl (Record name fields) =
  liftA2 Record (renameName name) (traverse renameName fields)

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
  -- make sure to rename the body first so that we don't collide with new
  -- names introduces by the argument declarations
  body' <- renameExpr body
  _     <- introduceVariables $ foldMap collectVariables [arg]
  arg'  <- renameExpr arg
  return $ Let arg' body'

renameExpr (Def name cases) = do
  liftA2 Def (renameName name) (traverse go_collectCase cases)
  where
    go_collectCase (args, body) = pushFrame $ do
      _ <- introduceVariables $ foldMap collectVariables args
      liftA2 (,) (traverse renameExpr args) (renameExpr body)

renameExpr (If cond then' else') =
  liftA3 If (renameExpr cond) (renameExpr then') (renameExpr else')

renameExpr (Lambda args body) =
  pushFrame $ liftA2 Lambda (introduceVariables args) (renameExpr body)

renameExpr (Block exprs) = pushFrame $ do
  _ <- introduceVariables $ foldMap go_collectDefNames exprs
  Block <$> traverse renameExpr exprs
  where
    go_collectDefNames (Def name _) = [name]
    go_collectDefNames _            = []

renameExpr (Match value caseExprs) = do
  liftA2 Match (renameExpr value) (traverse go_collectCaseExpr caseExprs)
  where
    go_collectCaseExpr (pattern, result) = pushFrame $ do
      _ <- introduceVariables $ collectVariables pattern
      liftA2 (,) (renameExpr pattern) (renameExpr result)

-- Helper functions

renameName :: Renaming m => Name -> m Name
renameName = lookupSymbolAlias

introduceVariables :: Renaming m => [Name] -> m [Name]
introduceVariables = uniq >=> traverse registerSymbol

collectVariables :: Expr -> [Name]
collectVariables (Identifier x) = [x]
collectVariables (Tuple      t) = foldMap collectVariables t
collectVariables _              = []

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
