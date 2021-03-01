module Flix.Compiler
  ( compile
  , compileT
  , runCompiler
  , runCompilerT
  ) where

import           Control.Monad                  ( (>=>) )
import           Core
import           Flix.Desugar                   ( desugar )
import           Flix.FlixMonad
import           Flix.FlixState
import           Flix.Parser                    ( parse )
import           Flix.Renamer                   ( rename )
import           Flix.Syntax                    ( AST )

runCompilerT :: Monad m => String -> m (Either String (CoreExpr, FlixState))
runCompilerT = flip runFlixMonadT makeEmptyState . compileT

runCompiler :: String -> Either String (CoreExpr, FlixState)
runCompiler = flip runFlixMonad makeEmptyState . compileT

compile :: String -> FlixMonad CoreExpr
compile = parseM >=> rename >=> desugar

compileT :: (Monad m) => String -> FlixMonadT m CoreExpr
compileT = parseM >=> rename >=> desugar

parseM :: (Monad m) => String -> FlixMonadT m AST
parseM = liftEitherToFlixM . parse
