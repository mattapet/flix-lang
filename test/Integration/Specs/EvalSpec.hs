module Integration.Specs.EvalSpec
  ( spec
  ) where

import           Control.Monad                  ( (>=>) )
import           Core.Builtin                   ( builtins )
import           Core.Expr
import           Core.Interpreter               ( eval )
import           Data.Types
import           Flix.Desugar                   ( desugar
                                                , makeEmptyState
                                                )
import           Flix.Parser                    ( parse )
import           Flix.Renamer                   ( rename )

import           Test.Hspec

spec :: Spec
spec = do
  describe "simple programs" $ do
    it "creates two variables in block and adds them" $ do
      let
        in'
          = " module TestModule \n\
            \ let x = 1         \n\
            \ let y = 2         \n\
            \ x + y"
      let out = LitV (Int 3)
      show <$> run in' `shouldBe` Right (show out)

    it "runs value through an identity function" $ do
      let
        in'
          = " module TestModule \n\
            \ let id a = a      \n\
            \ id 2"
      let out = LitV (Int 2)
      show <$> run in' `shouldBe` Right (show out)

  describe "recursion" $ do
    it "computes a factorial of 5" $ do
      let
        in'
          = " module TestModule                             \n\
            \ let factorial n =                             \n\
            \   if n < 2 then 1 else n * (factorial (n - 1))\n\
            \ factorial 5"
      let out = LitV (Int 120)
      show <$> run in' `shouldBe` Right (show out)

    it "computes a fib of 10" $ do
      let
        in'
          = " module TestModule                         \n\
            \ let fib n =                               \n\
            \   if n <= 2 then 1                        \n\
            \             else fib (n - 1) + fib (n - 2)\n\
            \ fib 10"
      let out = LitV (Int 55)
      show <$> run in' `shouldBe` Right (show out)

  describe "higher-order functions" $ do

    it "flips arguments of diff" $ do
      let
        in'
          = " module TestModule            \n\
            \ let flip f = { x y => f y x }\n\
            \ (flip (-)) 5 10"
      let out = LitV (Int 5)
      show <$> run in' `shouldBe` Right (show out)

  describe "pattern matching" $ do

    it "matches empty tuple" $ do
      let
        in'
          = " module TestModule                  \n\
            \ match () {                         \n\
            \   case () => true                  \n\
            \   case _ => false                  \n\
            \ }"
      let out = LitV (Bool True)
      show <$> run in' `shouldBe` Right (show out)

    it "matches 2-tuple (1, 2)" $ do
      let
        in'
          = " module TestModule                  \n\
            \ match (1, 2) {                     \n\
            \   case (2, 2) => true              \n\
            \   case (1, 2) => true              \n\
            \   case _ => false                  \n\
            \ }"
      let out = LitV (Bool True)
      show <$> run in' `shouldBe` Right (show out)

    it "matches 2-tuple (1, 2) with variable capture" $ do
      let
        in'
          = " module TestModule                  \n\
            \                                    \n\
            \ match (1, 2) {                     \n\
            \   case (2, 2) => true              \n\
            \   case (1, x) => x                 \n\
            \   case _ => false                  \n\
            \ }"
      let out = LitV (Int 2)
      show <$> run in' `shouldBe` Right (show out)

    it "matches nested tuple (1, (2, 3)) with variable capture" $ do
      let
        in'
          = " module TestModule                  \n\
            \ match (1, (2, 3)) {                \n\
            \   case (2, 2) => true              \n\
            \   case (x, (2, 3)) => x            \n\
            \ }"
      let out = LitV (Int 1)
      show <$> run in' `shouldBe` Right (show out)

    it "matches default underscore when no patterns are matched" $ do
      let
        in'
          = " module TestModule                   \n\
            \ match (1, 2) {                      \n\
            \   case (2, 2) => true               \n\
            \   case (x, (2, 3)) => x             \n\
            \   case _ => false                   \n\
            \ }"
      let out = LitV (Bool False)
      show <$> run in' `shouldBe` Right (show out)

    it "working with tuples like lists" $ do
      let
        in'
          = " module TestModule                   \n\
            \ record (:) head tail                \n\
            \ record Nil                          \n\
            \                                     \n\
            \ let head x = (x)                    \n\
            \ let tail x = match x {              \n\
            \   case () => ()                     \n\
            \   case (_:tail) => tail             \n\
            \ }                                   \n\
            \ let length xs = match xs {          \n\
            \    case () => 0                     \n\
            \    case (_:tail) => 1 + length tail \n\
            \ }                                   \n\
            \ length [1, 2, 3, 4, 5]"
      let out = LitV (Int 5)
      show <$> run in' `shouldBe` Right (show out)

    it "working with tuples like lists" $ do
      let
        in'
          = " module TestModule                   \n\
            \ let null () = true                  \n\
            \     null _  = false                 \n\
            \                                     \n\
            \ let xs = (1, (2, ()))               \n\
            \ null xs"
      let out = LitV (Bool False)
      show <$> run in' `shouldBe` Right (show out)

    it "working with tuples like lists" $ do
      let
        in'
          = " module TestModule                   \n\
            \ let null () = true                  \n\
            \     null _  = false                 \n\
            \                                     \n\
            \ let xs = ()                         \n\
            \ null xs                             "
      let out = LitV (Bool True)
      show <$> run in' `shouldBe` Right (show out)

    it "calculates list equality" $ do
      let
        in'
          = " module TestModule                   \n\
            \ let equals () () = true             \n\
            \     equals () _  = false            \n\
            \     equals _  () = false            \n\
            \     equals (x, xs) (y, ys) =        \n\
            \       if x == y then xs `equals` ys \n\
            \                 else false          \n\
            \                                     \n\
            \ let xs = (1, (2, (3, ())))          \n\
            \ let ys = (1, (2, (3, ())))          \n\
            \ xs `equals` ys                        "
      let out = LitV (Bool True)
      show <$> run in' `shouldBe` Right (show out)

    it "calculates list equality" $ do
      let
        in'
          = " module TestModule                   \n\
            \ let equals () () = true             \n\
            \     equals () _  = false            \n\
            \     equals _  () = false            \n\
            \     equals (x, xs)  (y, ys) =       \n\
            \       if x == y then xs `equals` ys \n\
            \                 else false          \n\
            \                                     \n\
            \ let xs = (1, (5, (3, ())))          \n\
            \ let ys = (1, (2, (3, ())))          \n\
            \ xs `equals` ys                        "
      let out = LitV (Bool False)
      show <$> run in' `shouldBe` Right (show out)

    it "calculates list equality" $ do
      let
        in'
          = " module TestModule                   \n\
            \ record (:) head tail                \n\
            \                                     \n\
            \ let equals () () = true             \n\
            \     equals () _  = false            \n\
            \     equals _  () = false            \n\
            \     equals (x : xs) (y : ys) =      \n\
            \       if x == y then xs `equals` ys \n\
            \                 else false          \n\
            \                                     \n\
            \ let xs = 1 : 2 : ()                 \n\
            \ let ys = 1 : 2 : 3 : ()             \n\
            \ xs `equals` ys                        "
      let out = LitV (Bool False)
      show <$> run in' `shouldBe` Right (show out)

    it "defines a list equality as operator ===" $ do
      let
        in'
          = " module TestModule                   \n\
            \ let (===) xs ys = match (xs, ys) {  \n\
            \     case ((), ()) => true           \n\
            \     case ((), _)  => false          \n\
            \     case (_,  ()) => false          \n\
            \     case ((x, xs), (y, ys)) =>      \n\
            \       if x == y then xs === ys      \n\
            \                 else false          \n\
            \    }                                \n\
            \ let xs = (1, (2, ()))               \n\
            \ let ys = (1, (2, (3, ())))          \n\
            \ xs === ys                           "
      let out = LitV (Bool False)
      show <$> run in' `shouldBe` Right (show out)

  describe "records" $ do
    it "Cons record declaration" $ do
      let
        in'
          = " module TestModule           \n\
          \   record Cons head tail       \n\
          \                               \n\
          \   let xs = Cons 1 (Cons 5 ()) \n\
          \                               \n\ 
          \   head (tail xs)"
      let out = LitV (Int 5)
      show <$> run in' `shouldBe` Right (show out)

    it "preserved constructor information of the values" $ do
      let
        in'
          = " module TestModule           \n\
          \   record (:) head tail        \n\
          \   record Nil                  \n\
          \                               \n\
          \   let xs = 1 : 5 : Nil        \n\
          \                               \n\ 
          \   tail (tail xs)              \n\
          \"
      let out =
            LambdaV (NominalTy "TestModule.Nil_$1") mempty "_$4" (Var "_$4")
      show <$> run in' `shouldBe` Right (show out)

    it "match on constructor type" $ do
      let
        in'
          = " module TestModule           \n\
          \   record (:) head tail        \n\
          \   record Nil                  \n\
          \                               \n\
          \   let xs = 1 : 5 : Nil        \n\
          \                               \n\ 
          \   match xs {                  \n\
          \     case (x:_) => x           \n\
          \   }                           \n\
          \"
      let out = LitV $ Int 1
      show <$> run in' `shouldBe` Right (show out)

    it "falls-through missed constructor type" $ do
      let
        in'
          = " module TestModule           \n\
          \   record (:) head tail        \n\
          \                               \n\ 
          \   match (1, 2) {              \n\
          \     case (x:_) => x           \n\
          \     case _ => false           \n\
          \   }                           \n\
          \"
      let out = LitV $ Bool False
      show <$> run in' `shouldBe` Right (show out)

    it "matches type recursively" $ do
      let
        in'
          = " module TestModule           \n\
          \   record (:) head tail        \n\
          \   record Nil                  \n\
          \                               \n\
          \   let xs = 1 : 2 : 3 : Nil    \n\
          \                               \n\ 
          \   match xs {                  \n\
          \     case (x:5:y:Nil) => x - y \n\
          \     case (x:2:y:Nil) => x + y \n\
          \     case _ => false           \n\
          \   }                           \n\
          \"
      let out = LitV $ Int 4
      show <$> run in' `shouldBe` Right (show out)

    it "matches constructor names" $ do
      let
        in'
          = " module TestModule            \n\
          \   record (:) head tail         \n\
          \   record Nil                   \n\
          \                                \n\
          \   let foldr f b Nil = b        \n\
          \       foldr f b (x : xs) =     \n\
          \             f (foldr f b xs) x \n\
          \                                \n\
          \   foldr (+) 0 [1, 2, 3]        \n\
          \"
      let out = LitV $ Int 6
      show <$> run in' `shouldBe` Right (show out)

run :: String -> Either String Value
run = parse >=> rename >=> desugar makeEmptyState >=> (fst <$>) . unpack
  where
    unpack (core, constrs) = eval (Environment b constrs) core
    (Environment b _) = builtins

