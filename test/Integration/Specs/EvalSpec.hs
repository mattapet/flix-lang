module Integration.Specs.EvalSpec
  ( spec
  ) where

import           Control.Monad                  ( (>=>) )
import           Eval.Builtin                   ( builtins )
import           Eval.Core
import           Eval.Interpreter               ( eval )
import           Syntax.Desugar                 ( desugar )
import           Syntax.Parser                  ( parse )
import           Syntax.Renamer                 ( rename )

import           Test.Hspec

spec :: Spec
spec = do
  describe "simple script-like programs" $ do
    it "creates two variables in block and adds them" $ do
      let
        in'
          = "{\n\
                 \  let x = 1\n\
                 \  let y = 2\n\
                 \  x + y\n\
                 \}"
      let out = LitV (Int 3)
      show <$> run in' `shouldBe` Right (show out)

    it "runs value through an identity function" $ do
      let
        in'
          = "{\n\
              \  let id a = a\n\
              \  id 2\n\
              \}"
      let out = LitV (Int 2)
      show <$> run in' `shouldBe` Right (show out)

    it "computes a factorial of 5" $ do
      let
        in'
          = "{\n\
              \  let factorial n = \n\
              \    if n < 2 then 1 else n * (factorial (n - 1))\n\
              \  factorial 5\n\
              \}"
      let out = LitV (Int 120)
      show <$> run in' `shouldBe` Right (show out)

    it "computes a fib of 10" $ do
      let
        in'
          = "{\n\
              \  let fib n = \n\
              \    if n <= 2 then 1 \n\
              \              else fib (n - 1) + fib (n - 2)\n\
              \  fib 10\n\
              \}"
      let out = LitV (Int 55)
      show <$> run in' `shouldBe` Right (show out)

    it "flips arguments of diff" $ do
      let
        in'
          = "{\n\
              \  let flip f = { x y => f y x }\n\
              \  let diff x y = x - y         \n\
              \  (flip diff) 5 10\n\
              \}"
      let out = LitV (Int 5)
      show <$> run in' `shouldBe` Right (show out)


run :: String -> Either String Value
run = parse >=> rename >=> desugar >=> (fst <$>) . eval builtins

