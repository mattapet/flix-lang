module Syntax.Specs.ParserSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import           Syntax.Core
import           Syntax.Parser                  ( parse )
import           Test.Hspec
import           Text.Printf                    ( printf )

spec :: Spec
spec = do
  describe "Expressions" $ do
    describe "Basic expressions" $ do
      let
        testSuites =
          [ ("true" , Right $ Expr $ BoolLiteral True)
          , ("false", Right $ Expr $ BoolLiteral False)
          , ("1"    , Right $ Expr $ NumberLiteral 1)
          , ("-1"   , Right $ Expr $ NumberLiteral (-1))
          , ("(1)"  , Right $ Expr $ NumberLiteral 1)
          , ("x"    , Right $ Expr $ Identifier "x")
          , ("x'"   , Right $ Expr $ Identifier "x'")
          , ("a_b"  , Right $ Expr $ Identifier "a_b")
          , ("f a"  , Right $ Expr $ Call (Identifier "f") [Identifier "a"])
          , ( "f a b"
            , Right $ Expr $ Call (Identifier "f")
                                  [Identifier "a", Identifier "b"]
            )
          , ( "x+y+z"
            , Right $ Expr $ BinOp
              "+"
              (BinOp "+" (Identifier "x") (Identifier "y"))
              (Identifier "z")
            )
          , ("let x = 2"  , Right $ Expr $ Let "x" [] (NumberLiteral 2))
          , ("let f a = a", Right $ Expr $ Let "f" ["a"] (Identifier "a"))
          , ("{ 2 }"      , Right $ Expr $ Block [NumberLiteral 2])
          , ( "let f a = { a }"
            , Right $ Expr $ Let "f" ["a"] (Block [Identifier "a"])
            )
          , ( "{\n\
            \  let x = 2\n\
            \  x\n\
            \}"
            , Right $ Expr $ Block
              [Let "x" [] (NumberLiteral 2), Identifier "x"]
            )
          , ( "if true then 1 else 2"
            , Right $ Expr $ If (BoolLiteral True)
                                (NumberLiteral 1)
                                (NumberLiteral 2)
            )
          , ( "if true then x else y"
            , Right $ Expr $ If (BoolLiteral True)
                                (Identifier "x")
                                (Identifier "y")
            )
          , ( "if true\n\
              \  then 1\n\
              \  else 0"
            , Right $ Expr $ If (BoolLiteral True)
                                (NumberLiteral 1)
                                (NumberLiteral 0)
            )
          , ( "{\n\
              \  let x = 2\n\
              \  let y = 4\n\
              \  x + y\n\
              \}"
            , Right $ Expr $ Block
              [ Let "x" [] (NumberLiteral 2)
              , Let "y" [] (NumberLiteral 4)
              , BinOp "+" (Identifier "x") (Identifier "y")
              ]
            )
          , ("let id a = a", Right $ Expr $ Let "id" ["a"] (Identifier "a"))
          , ( "let factorial n = if n < 1 then 1 else n * factorial (n - 1)"
            , Right $ Expr $ Let
              "factorial"
              ["n"]
              (If
                (BinOp "<" (Identifier "n") (NumberLiteral 1))
                (NumberLiteral 1)
                (BinOp
                  "*"
                  (Identifier "n")
                  (Call (Identifier "factorial")
                        [BinOp "-" (Identifier "n") (NumberLiteral 1)]
                  )
                )
              )
            )
          , ( "let factorial n = {\n\
            \  let factorial' n acc = if n < 1\n\
            \    then 1\n\
            \    else factorial' (n - 1) (n * acc)\n\
            \  factorial' n 1\n\
            \}"
            , Right $ Expr $ Let
              "factorial"
              ["n"]
              (Block
                [ Let "factorial'" ["n", "acc"] $ If
                  (BinOp "<" (Identifier "n") (NumberLiteral 1))
                  (NumberLiteral 1)
                  (Call
                    (Identifier "factorial'")
                    [ BinOp "-" (Identifier "n") (NumberLiteral 1)
                    , BinOp "*" (Identifier "n") (Identifier "acc")
                    ]
                  )
                , Call (Identifier "factorial'")
                       [Identifier "n", NumberLiteral 1]
                ]
              )
            )
          ]
      forM_ testSuites $ \(in', out) ->
        it (printf "parses %s into %s" (show in') (show out)) $ do
          parse in' `shouldBe` out

      describe "operator binary operator parsing" $ do
        let testSuites =
              ["+", "-", "*", "/", "==", "<", ">", "<=", ">=", "&&", "||"]
        forM_ testSuites $ \op ->
          it (printf "parses binary operator '%s'" (show op)) $ do
            let result = BinOp op (Identifier "x") (Identifier "y")
            parse (printf "x %s y" op) `shouldBe` Right (Expr result)

{-
  record (:) head tail
  record ([])

  let length xs = {
    let length' acc ([]) = acc
    let length' acc (_:xs) = length xs (acc + 1)
    length' xs
  }

  let id a = a;
  let const a = { _ => a }

  let fib n = if n < 2
    then 1
    else (fib n - 1) + (fib n - 2)

  let factorial n = {
    let factorial' n acc = if n < 1
      then acc
      else factorial' (n - 1) (acc * n)
    factorial' n 1
  }
  |
  |
  v
  let factorial n = {
    let factorial' n acc = match n < 1 {
      case true  => acc
      case false => factorial' (n - 1) (acc * n)
    }
    factorial' n 1
  }
-}
