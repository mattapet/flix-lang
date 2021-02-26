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
  , makeBoolBuiltin "&&" (&&)
  , makeBoolBuiltin "||" (||)
  , equals
  , nequals
  , makeCompareBuiltin "<"  (<)
  , makeCompareBuiltin ">"  (>)
  , makeCompareBuiltin "<=" (<=)
  , makeCompareBuiltin ">=" (>=)
  ]

equals :: (Name, Value)
equals = makeBuiltinBinop "==" (\l r -> Right $ Bool $ l == r)

nequals :: (Name, Value)
nequals = makeBuiltinBinop "!=" (\l r -> Right $ Bool $ l /= r)

makeIntBuiltin :: Name -> (Integer -> Integer -> Integer) -> (Name, Value)
makeIntBuiltin name op = makeBuiltinBinop name apply
  where
    apply (Int x) (Int y) = return $ Int $ x `op` y
    apply (Int _) y       = throwError y
    apply x       _       = throwError x

    throwError arg =
      Left
        $  "Function '"
        ++ name
        ++ "' expects integer values but '"
        ++ show arg
        ++ "' was provided"

makeCompareBuiltin :: Name -> (Integer -> Integer -> Bool) -> (Name, Value)
makeCompareBuiltin name op = makeBuiltinBinop name apply
  where
    apply (Int x) (Int y) = return $ Bool $ x `op` y
    apply (Int _) y       = throwError y
    apply x       _       = throwError x

    throwError arg =
      Left
        $  "Function '"
        ++ name
        ++ "' expects integer values but '"
        ++ show arg
        ++ "' was provided"

makeBoolBuiltin :: Name -> (Bool -> Bool -> Bool) -> (Name, Value)
makeBoolBuiltin name op = makeBuiltinBinop name apply
  where
    apply (Bool x) (Bool y) = return $ Bool $ x `op` y
    apply (Bool _) y        = throwError y
    apply x        _        = throwError x
    throwError arg =
      Left
        $  "Function '"
        ++ name
        ++ "' expects boolean values but '"
        ++ show arg
        ++ "' was provided"

makeBuiltinBinop :: Name
                 -> (Literal -> Literal -> Either String Literal)
                 -> (Name, Value)
makeBuiltinBinop name op = (name, BuiltinV name evalLhs)
  where
    evalLhs (LitV x) = return $ BuiltinV name (evalRhs x)
    evalLhs x        = throwError x

    evalRhs x (LitV y) = LitV <$> x `op` y
    evalRhs _ y        = throwError y
    throwError arg =
      Left
        $  "Function '"
        ++ name
        ++ "' expects literal values but '"
        ++ show arg
        ++ "' was provided"