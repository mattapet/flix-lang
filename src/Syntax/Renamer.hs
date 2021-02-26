{-# LANGUAGE TupleSections #-}

module Syntax.Renamer
  ( rename
  ) where

import           Control.Applicative            ( liftA2
                                                , liftA3
                                                )
import           Control.Monad                  ( (>=>) )
import           Control.Monad.StateT
import           Data.Functor                   ( ($>) )
import           Data.List                      ( intercalate
                                                , intersect
                                                )
import           Data.Map                       ( (!?)
                                                , Map
                                                , empty
                                                , insert
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Syntax.Core


type Result a = StateT RenamerState (Either String) a

-- Renaming State
data RenamerState = RenamerState
  { state_substitutions     :: [(Name, Name)]
  , state_uniqueNameCounter :: Map String Int
  }

makeEmptyState :: RenamerState
makeEmptyState = RenamerState [] empty

getSubstitutions :: Result [(Name, Name)]
getSubstitutions = state_substitutions <$> get

setSubstitutions :: [(Name, Name)] -> Result ()
setSubstitutions subs = do
  counts <- getNameCounter
  put $ RenamerState subs counts

getNameCounter :: Result (Map String Int)
getNameCounter = state_uniqueNameCounter <$> get

setNameCounter :: Map String Int -> Result ()
setNameCounter counts = do
  subs <- getSubstitutions
  put $ RenamerState subs counts

incrementCountForName :: Name -> Result Int
incrementCountForName x = do
  counts <- getNameCounter
  let nextId = maybe 1 (+ 1) $ counts !? x
  setNameCounter $ insert x nextId counts
  return nextId

bindNameSub :: Name -> Name -> Result ()
bindNameSub name sub = do
  subs <- getSubstitutions
  setSubstitutions $ (name, sub) : subs

-- Renaming

rename :: AST -> Either String AST
rename = (fst <$>) . flip runStateT makeEmptyState . rename'

rename' :: AST -> Result AST
rename' (Expr e) = Expr <$> renameExpr e

-- Renaming Expressions

renameExpr :: Expr -> Result Expr
renameExpr Underscore          = return Underscore
renameExpr val@BoolLiteral{}   = return val
renameExpr val@NumberLiteral{} = return val
renameExpr (Identifier x)      = Identifier . lookupVar <$> getSubstitutions
  where lookupVar subs = fromMaybe x $ lookup x subs

renameExpr (BinOp op lhs rhs) =
  liftA2 (BinOp op) (renameExpr lhs) (renameExpr rhs)

renameExpr (Call callee args) =
  liftA2 Call (renameExpr callee) (traverse renameExpr args)

renameExpr (Let name args body) = do
  name' <- introduceVariable name -- Introduce new name of the current level
  pushFrame $ liftA2 (Let name') (introduceVariables args) (renameExpr body)

renameExpr (If cond then' else') =
  liftA3 If (renameExpr cond) (renameExpr then') (renameExpr else')

renameExpr (Lambda args body) =
  pushFrame $ liftA2 Lambda (introduceVariables args) (renameExpr body)

renameExpr (Block exprs) = pushFrame $ Block <$> traverse renameExpr exprs

renameExpr (Match value caseExprs) =
  liftA2 Match (renameExpr value) (traverse renameCaseExpr caseExprs)

renameCaseExpr :: CaseExpr -> Result CaseExpr
renameCaseExpr (pattern, value) =
  liftA2 (,) (renameExpr pattern) (renameExpr value)

-- Helper functions

pushFrame :: Result a -> Result a
pushFrame f = do
  pre    <- getSubstitutions
  result <- f
  setSubstitutions pre $> result

introduceVariables :: [Name] -> Result [Name]
introduceVariables = uniq >=> traverse introduceVariable

introduceVariable :: Name -> Result Name
introduceVariable x = do
  nextId <- incrementCountForName x
  let x' = x ++ "_$" ++ show nextId
  bindNameSub x x'
  return x'

uniq :: [Name] -> Result [Name]
uniq names = case conflictingNames names [] of
  []        -> return names
  conflicts -> fail $ formatConflicts conflicts
  where
    conflictingNames [] acc = acc
    conflictingNames (x : xs) acc =
      conflictingNames xs (acc ++ [x] `intersect` xs)

formatConflicts :: [Name] -> String
formatConflicts = wrapInMessage . sep . (quote <$>)
  where
    sep           = intercalate ", "
    quote         = ("'" ++) . (++ "'")
    wrapInMessage = ("Conflicting definition for symbols " ++)
