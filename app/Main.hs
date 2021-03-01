module Main where

import           Control.Monad                  ( (>=>) )
import           Eval.Builtin
import           Eval.Core                      ( Environment(..) )
import           Eval.Interpreter
import           Syntax.Core
import           Syntax.Desugar
import           Syntax.Parser
import           Syntax.Renamer

import           System.Environment

main :: IO ()
main = do
  [filename] <- getArgs
  input      <- readFile filename
  case run input of
    Right x -> print $ show x
    Left  x -> print x
  where
    run = parse >=> rename >=> desugar makeEmptyState >=> (fst <$>) . unpack
    unpack (core, constrs) = eval (Environment b constrs) core
    (Environment b _) = builtins
