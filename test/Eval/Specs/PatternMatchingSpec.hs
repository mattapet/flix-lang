module Eval.Specs.PatternMatchingSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import           Eval.Builtin
import           Eval.Core
import           Eval.Interpreter               ( eval )
import           Test.Hspec
import           Text.Printf                    ( printf )

spec :: Spec
spec = do
  describe "Literal matching" $ do
    let
      testSuite =
        [ ([(DefaultP, Lit (Bool True))]    , Lit (Int 1), LitV (Bool True))
        , ([(LitP (Int 1), Lit (Bool True))], Lit (Int 1), LitV (Bool True))
        , ( [(LitP (Bool True), Lit (Bool True))]
          , Lit (Bool True)
          , LitV (Bool True)
          )
        , ( [(LitP (Bool True), Lit (Bool False)), (DefaultP, Lit (Bool True))]
          , Lit (Int 2)
          , LitV (Bool True)
          )
        ]

    forM_ testSuite $ \(ptr, in', out) ->
      it (printf "should match %s on pattern %s" (show in') (show ptr)) $ do
        show . fst <$> eval builtins (Case in' ptr) `shouldBe` Right (show out)

  describe "Variable matching" $ do
    let testSuite = [([(VarP "x", Var "x")], Lit (Int 1), LitV (Int 1))]

    forM_ testSuite $ \(ptr, in', out) ->
      it (printf "should match %s on pattern %s" (show in') (show ptr)) $ do
        show . fst <$> eval builtins (Case in' ptr) `shouldBe` Right (show out)

