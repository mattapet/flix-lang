module Core.Specs.PatternMatchingSpec
  ( spec
  ) where

import           Core.Builtin
import           Core.Expr
import           Core.Interpreter               ( eval )
import           Data.Map                       ( empty
                                                , fromList
                                                )
import           Data.Types
import           Test.Hspec
import           Test.Util

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

    it "matches tuple (1, 2, 3)" $ do
      let in' = Lam
            "p"
            (App (App (App (Var "p") (Lit $ Int 1)) (Lit $ Int 2)) (Lit $ Int 3)
            )
      let
        ptr =
          [(TupleP [LitP $ Int 1, LitP $ Int 2, LitP $ Int 3], Lit $ Bool True)]
      let out = LitV $ Bool True
      show . fst <$> eval builtins (Case in' ptr) `shouldBe` Right (show out)

