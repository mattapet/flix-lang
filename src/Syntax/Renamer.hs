module Syntax.Renamer
  ( rename
  ) where

import           Control.Monad                  ( (>=>) )
import           Control.Monad.StateT
import           Data.List                      ( intercalate
                                                , intersect
                                                )
import           Data.Map                hiding ( drop
                                                , lookup
                                                )
import           Data.Maybe
import           Syntax.Core

type Env = Map String Int
type Names = [(Name, Name)]
type Result a = StateT Env (Either String) a

rename :: AST -> Either String AST
rename = (fst <$>) . flip runStateT empty . rename' []

rename' :: Names -> AST -> Result AST
rename' _  val@BoolLiteral{}   = return val
rename' _  val@NumberLiteral{} = return val

rename' ns (Identifier x)      = return $ Identifier x'
  where x' = fromMaybe x $ lookup x ns

rename' ns (BinOp op lhs rhs) = do
  lhs' <- rename' ns lhs
  rhs' <- rename' ns rhs
  return $ BinOp op lhs' rhs'

rename' ns (Call callee args) = do
  callee' <- rename' ns callee
  args'   <- traverse (rename' ns) args
  return $ Call callee' args'

rename' ns (Let name args body) = do
  (name' : args', body') <- renameContext
  return $ Let name' args' body'
  where
    renameContext = do
      ns'   <- renameVariables (name : args)
      body' <- (rename' $ ns' ++ ns) body
      let vars' = snd <$> ns'
      return (vars', body')

rename' ns (If cond then' else') = do
  cond'  <- rename' ns cond
  then'' <- rename' ns then'
  else'' <- rename' ns else'
  return $ If cond' then'' else''

rename' ns (Block exprs) = Block <$> renameBlock ns exprs

-- Helper functions

renameBlock :: [(Name, Name)] -> [AST] -> Result [AST]
renameBlock _  [] = return []
renameBlock ns (Let name args body : remainingBlock) = do
  (let', ns')     <- renameLet
  remainingBlock' <- renameBlock ns' remainingBlock
  return $ let' : remainingBlock'
  where
    renameLet = do
      ns'   <- renameVariables (name : args)
      body' <- (rename' $ ns' ++ ns) body
      let (name' : args') = snd <$> ns'
      let renamedLetName  = head ns'
      return (Let name' args' body', renamedLetName : ns)

renameBlock ns (x : xs) = do
  x' <- rename' ns x
  (x' :) <$> renameBlock ns xs

renameVariables :: [Name] -> Result [(Name, Name)]
renameVariables = uniqueNames >=> traverse renameVariable
  where
    renameVariable x = do
      env <- get
      let nextId = maybe 1 (+ 1) $ env !? x
      let x'     = x ++ "_$" ++ show nextId
      put (insert x nextId env)
      return (x, x')

uniqueNames :: [Name] -> Result [Name]
uniqueNames names = case conflictingNames names [] of
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
