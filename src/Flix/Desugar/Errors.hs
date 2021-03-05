module Flix.Desugar.Errors where

import           Flix.Syntax                    ( Expr )

defNumberOfArgumentsError :: String
defNumberOfArgumentsError = "Def bindings with different amount of arguments"

emptyBlockError :: String
emptyBlockError = "Unexpected empty block"

letAtEndOfBlockError :: String
letAtEndOfBlockError = "Illegal binding at the end of the block"

unsupportedPatternMatchExprError :: Expr -> String
unsupportedPatternMatchExprError e =
  "Unsupported pattern match expr '" ++ show e ++ "'"
