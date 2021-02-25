module Eval.Builtin
  ( builtins
  ) where

import           Data.Map
import           Eval.Core

builtins :: Environment
builtins = fromList
  [ makeIntBuiltin "+" (+)
  , makeIntBuiltin "-" (-)
  , makeIntBuiltin "*" (*)
  , makeIntBuiltin "/" div
  , makeIntBuiltin "%" mod
  ]

makeIntBuiltin :: Name -> (Integer -> Integer -> Integer) -> (Name, Value)
makeIntBuiltin name op = (name, BuiltinV name evalLhs)
  where
    evalLhs (LitV (Int x)) = return $ BuiltinV "+" (evalRhs x)
    evalRhs x (LitV (Int y)) = return $ LitV $ Int $ x `op` y
