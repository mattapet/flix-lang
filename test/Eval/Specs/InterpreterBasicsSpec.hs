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
  describe "substitution" $ do
    let
      testSuite =
        [ (App (Lam "x" (Var "x")) (Lit (Int 1)), LitV (Int 1))
        , ( App (Lam "a" (Var "a")) (Lam "c" (Var "c"))
          , LambdaV empty "c" (Var "c")
          )
        , ( App (Lam "f" (App (Var "f") (Lit (Int 1))))
                (App (Var "+") (Lit (Int 1)))
          , LitV (Int 2)
          )
        , (Bind ("x", Lit (Int 1)) (Var "x"), LitV (Int 1))
        , ( App
            (Bind ("x", Lit (Int 1))
                  (Lam "y" (App (App (Var "+") (Var "x")) (Var "y")))
            )
            (Lit (Int 1))
          , LitV (Int 2)
          )
        ]
    forM_ testSuite $ \(in', out) ->
      it (printf "evaluates %s to %s" (show in') (show out)) $ do
        show . fst <$> eval builtins in' `shouldBe` Right (show out)

  describe "primitives" $ do
    let testSuite =
          [ (App (App (Var "+") (Lit (Int 1))) (Lit (Int 1)), LitV (Int 2))
          , (App (App (Var "-") (Lit (Int 1))) (Lit (Int 1)), LitV (Int 0))
          , (App (App (Var "*") (Lit (Int 1))) (Lit (Int 4)), LitV (Int 4))
          , (App (App (Var "/") (Lit (Int 6))) (Lit (Int 2)), LitV (Int 3))
          , (App (App (Var "%") (Lit (Int 5))) (Lit (Int 3)), LitV (Int 2))
          ]
    forM_ testSuite $ \(in', out) ->
      it (printf "evaluates %s to %s" (show in') (show out)) $ do
        show . fst <$> eval builtins in' `shouldBe` Right (show out)

  describe "invalid evaluations" $ do
    let tesSuite =
          [ ( App (Lit (Int 1)) (Lit (Int 1))
            , "Expression 'LitV Int 1' is not callable"
            )
          ]
    forM_ tesSuite $ \(in', out') ->
      it (printf "returns a failure on %s" (show in')) $ do
        case eval builtins in' of
          Right _ ->
            fail $ printf "Expression %s expected to fail but didn't" (show in')
          Left out -> out `shouldBe` out'
