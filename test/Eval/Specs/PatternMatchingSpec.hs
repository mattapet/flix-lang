module Eval.Specs.PatternMatchingSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import           Data.Map                       ( empty )
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

  describe "Tuple matching" $ do
    it "matches tuple (1, 2)" $ do
      let in' = Lam "p" (App (App (Var "p") (Lit $ Int 1)) (Lit $ Int 2))
      let ptr = [(TupleP [LitP $ Int 1, LitP $ Int 2], Lit $ Bool True)]
      let out = LitV $ Bool True
      show . fst <$> eval builtins (Case in' ptr) `shouldBe` Right (show out)

