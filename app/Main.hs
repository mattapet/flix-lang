module Main where

import           Control.Monad                  ( (>=>) )
import           Core.Builtin
import           Core.Expr                      ( Environment(..) )
import           Core.Interpreter
import           Flix.Desugar
import           Flix.Parser
import           Flix.Renamer
import           Flix.Syntax

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
