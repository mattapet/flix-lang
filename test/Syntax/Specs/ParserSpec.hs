module Syntax.Specs.ParserSpec
  ( spec
  ) where

import           Syntax.Core
import           Syntax.Parser                  ( parse )
import           Test.Hspec
import           Test.Util

spec :: Spec
spec = do
  describe "Expressions" $ do
    describe "Basic expressions" $ do
      let
        testSuite =
          [ ("_"     , Underscore)
          , ("true"  , BoolLiteral True)
          , ("false" , BoolLiteral False)
          , ("trueF" , Identifier "trueF")
          , ("false" , BoolLiteral False)
          , ("1"     , NumberLiteral 1)
          , ("-1"    , NumberLiteral (-1))
          , ("(1)"   , NumberLiteral 1)
          , ("()"    , Tuple [])
          , ("(1, 2)", Tuple [NumberLiteral 1, NumberLiteral 2])
          , ("x"     , Identifier "x")
          , ("x'"    , Identifier "x'")
          , ("a_b"   , Identifier "a_b")
          , ("f a"   , Call (Identifier "f") [Identifier "a"])
          , ("f a b", Call (Identifier "f") [Identifier "a", Identifier "b"])
          , ( "x+y+z"
            , BinOp "+"
                    (BinOp "+" (Identifier "x") (Identifier "y"))
                    (Identifier "z")
            )
          , ("(+)"        , OperatorCapture "+")
          , ("let x = 2"  , LetMatch "x" [([], NumberLiteral 2)])
          , ("let f a = a", LetMatch "f" [([Identifier "a"], Identifier "a")])
          , ("{ 2 }"      , Block [NumberLiteral 2])
          , ( "let f a = { a }"
            , LetMatch "f" [([Identifier "a"], Block [Identifier "a"])]
            )
          , ( "{\n\
            \  let x = 2\n\
            \  x\n\
            \}"
            , Block [LetMatch "x" [([], NumberLiteral 2)], Identifier "x"]
            )
          , ( "if true then 1 else 2"
            , If (BoolLiteral True) (NumberLiteral 1) (NumberLiteral 2)
            )
          , ( "if true then x else y"
            , If (BoolLiteral True) (Identifier "x") (Identifier "y")
            )
          , ( "if true\n\
              \  then 1\n\
              \  else 0"
            , If (BoolLiteral True) (NumberLiteral 1) (NumberLiteral 0)
            )
          , ( "{\n\
              \  let x = 2\n\
              \  let y = 4\n\
              \  x + y\n\
              \}"
            , Block
              [ LetMatch "x" [([], NumberLiteral 2)]
              , LetMatch "y" [([], NumberLiteral 4)]
              , BinOp "+" (Identifier "x") (Identifier "y")
              ]
            )
          , ("let id a = a", LetMatch "id" [([Identifier "a"], Identifier "a")])
          , ( "let factorial n = if n < 1 then 1 else n * factorial (n - 1)"
            , LetMatch
              "factorial"
              [ ( [Identifier "n"]
                , If
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
              ]
            )
          , ( "let factorial n = {\n\
            \  let factorial' n acc = if n < 1\n\
            \    then 1\n\
            \    else factorial' (n - 1) (n * acc)\n\
            \  factorial' n 1\n\
            \}"
            , LetMatch
              "factorial"
              [ ( [Identifier "n"]
                , Block
                  [ LetMatch
                    "factorial'"
                    [ ( [Identifier "n", Identifier "acc"]
                      , If
                        (BinOp "<" (Identifier "n") (NumberLiteral 1))
                        (NumberLiteral 1)
                        (Call
                          (Identifier "factorial'")
                          [ BinOp "-" (Identifier "n") (NumberLiteral 1)
                          , BinOp "*" (Identifier "n") (Identifier "acc")
                          ]
                        )
                      )
                    ]
                  , Call (Identifier "factorial'")
                         [Identifier "n", NumberLiteral 1]
                  ]
                )
              ]
            )
          ]
      forM_ testSuite $ \(in', out) ->
        it (printf "parses %s into %s" (show in') (show out)) $ do
          parse in' `shouldBe` Right (Expr out)

      describe "Match expressions" $ do
        let
          testSuite =
            [ ( "match 1 {\n\
                \  case true  => 1\n\
                \  case false => 0\n\
                \}"
              , Match
                (NumberLiteral 1)
                [ (BoolLiteral True , NumberLiteral 1)
                , (BoolLiteral False, NumberLiteral 0)
                ]
              )
            , ( "match x {\n\
                \  case true => {\n\
                \    let x = 123\n\
                \    x\n\
                \  }\n\
                \  case _ => 0\n\
                \}"
              , Match
                (Identifier "x")
                [ ( BoolLiteral True
                  , Block
                    [LetMatch "x" [([], NumberLiteral 123)], Identifier "x"]
                  )
                , (Underscore, NumberLiteral 0)
                ]
              )
            ]
        forM_ testSuite $ \(in', out) ->
          it (printf "parses match expression %s" (show in')) $ do
            parse in' `shouldBe` Right (Expr out)

      describe "Lambda expressions" $ do
        let
          testSuite =
            [ ("{ x => x }", Lambda ["x"] (Identifier "x"))
            , ( "{ x y => x + y }"
              , Lambda ["x", "y"] (BinOp "+" (Identifier "x") (Identifier "y"))
              )
            , ( "{ x y => {\n\
                  \  let sum = x + y\n\
                  \  sum\n\
                  \}}"
              , Lambda
                ["x", "y"]
                (Block
                  [ LetMatch
                    "sum"
                    [([], BinOp "+" (Identifier "x") (Identifier "y"))]
                  , Identifier "sum"
                  ]
                )
              )
            , ( "{ x => x } 2"
              , Call (Lambda ["x"] (Identifier "x")) [NumberLiteral 2]
              )
            ]
        forM_ testSuite $ \(in', out) ->
          it (printf "parses lambda %s" (show in')) $ do
            parse in' `shouldBe` Right (Expr out)

      describe "operator binary operator parsing" $ do
        let testSuite =
              ["+", "-", "*", "/", "==", "<", ">", "<=", ">=", "&&", "||"]
        forM_ testSuite $ \op ->
          it (printf "parses binary operator '%s'" (show op)) $ do
            let result = BinOp op (Identifier "x") (Identifier "y")
            parse (printf "x %s y" op) `shouldBe` Right (Expr result)

      describe "pattern matching let expressions" $ do
        let testSuite =
              [ ("let x () = 0", LetMatch "x" [([Tuple []], NumberLiteral 0)])
              , ( "let x (1, 2) = 3 \n\
              \        x  _     = -1"
                , LetMatch
                  "x"
                  [ ( [Tuple [NumberLiteral 1, NumberLiteral 2]]
                    , NumberLiteral 3
                    )
                  , ([Underscore], NumberLiteral (-1))
                  ]
                )
              ]
        forM_ testSuite $ \(in', out) ->
          it (printf "parses %s to %s" (show in') (show out)) $ do
            parse in' `shouldBe` Right (Expr out)

  describe "Declarations" $ do
    let testSuite =
          [ ("module TestModule", Module "TestModule" [])
          , ( "module TestModule\nlet x = 1"
            , Module "TestModule" [Expr $ LetMatch "x" [([], NumberLiteral 1)]]
            )
          , ("record Nil"           , Record "Nil" [])
          , ("record Cons head tail", Record "Cons" ["head", "tail"])
          ]
    forM_ testSuite $ \(in', out) ->
      it (printf "parses %s to %s" (show in') (show out)) $ do
        parse in' `shouldBe` Right (Decl out)

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
