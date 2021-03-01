module Main where

import           Control.Monad                  ( (>=>) )
import           Core.Builtin
import           Core.Expr                      ( Environment(..) )
import           Core.Interpreter
import           Flix                           ( runCompiler )
import           Flix.FlixState

import           System.Environment

main :: IO ()
main = do
  [filename] <- getArgs
  input      <- readFile filename
  print' $ run input
  where
    run = runCompiler >=> (fst <$>) . eval'
    eval' (core, FlixState _ _ _ constrs _) = eval (Environment b constrs) core
    (Environment b _) = builtins

    print' (Right x) = print $ show x
    print' (Left  x) = print x
