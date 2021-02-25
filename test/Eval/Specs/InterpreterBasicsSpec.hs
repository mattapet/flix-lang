module Eval.Specs.InterpreterBasicsSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import           Data.Map                       ( empty )
import           Eval.Builtin                   ( builtins )
import           Eval.Core
import           Eval.Interpreter               ( eval )
import           Test.Hspec
import           Text.Printf                    ( printf )


spec :: Spec
spec = do
  let testSuite =
        [ (Lit (Bool True), LitV (Bool True))
        , (Lit (Int 1)    , LitV (Int 1))
        , (App (App (Var "+") (Lit (Int 1))) (Lit (Int 1)), LitV (Int 2))
        , (App (App (Var "-") (Lit (Int 1))) (Lit (Int 1)), LitV (Int 0))
        , (App (App (Var "*") (Lit (Int 1))) (Lit (Int 4)), LitV (Int 4))
        , (App (App (Var "/") (Lit (Int 6))) (Lit (Int 2)), LitV (Int 3))
        , (App (App (Var "%") (Lit (Int 5))) (Lit (Int 3)), LitV (Int 2))
        ]
  forM_ testSuite $ \(in', out) ->
    it (printf "evaluates %s to %s" (show in') (show out)) $ do
      show . fst <$> eval builtins in' `shouldBe` Right (show out)
