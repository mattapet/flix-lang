module Syntax.Specs.DesugarSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Eval.Core                     as C
import qualified Syntax.Core                   as S
import           Syntax.Desugar                 ( desugar )
import           Test.Hspec
import           Text.Printf                    ( printf )

spec :: Spec
spec = do
  describe "Atoms" $ do
    let
      testSuite =
        [ (S.BoolLiteral True   , C.Lit (C.Bool True))
        , (S.NumberLiteral 1    , C.Lit (C.Int 1))
        , (S.Identifier "x"     , C.Var "x")
        , (S.OperatorCapture "+", C.Var "+")
        , (S.Tuple []           , C.Lam "_$1" (C.Var "_$1"))
        , ( S.Tuple [S.Identifier "x", S.Identifier "y"]
          , C.Lam "_$1" (C.App (C.App (C.Var "_$1") (C.Var "x")) (C.Var "y"))
          )
        , (S.Lambda ["x"] (S.Identifier "x"), C.Lam "x" (C.Var "x"))
        , ( S.Lambda ["x", "y"] (S.Identifier "x")
          , C.Lam "x" (C.Lam "y" (C.Var "x"))
          )
        ]
    forM_ testSuite $ \(in', out) ->
      it (printf "translates %s to %s" (show in') (show out)) $ do
        desugar (S.Expr in') `shouldBe` Right out

  describe "Calls" $ do
    let testSuite =
          [ (S.Call (S.Identifier "f") [], C.Var "f")
          , ( S.Call (S.Identifier "f") [S.Identifier "a"]
            , C.App (C.Var "f") (C.Var "a")
            )
          , ( S.Call (S.Identifier "f") [S.Identifier "a", S.NumberLiteral 4]
            , C.App (C.App (C.Var "f") (C.Var "a")) (C.Lit $ C.Int 4)
            )
          , ( S.Call (S.Lambda ["x"] (S.Identifier "x")) [S.NumberLiteral 1]
            , C.App (C.Lam "x" (C.Var "x")) (C.Lit (C.Int 1))
            )
          , ( S.BinOp "+" (S.Identifier "x") (S.Identifier "y")
            , C.App (C.App (C.Var "+") (C.Var "x")) (C.Var "y")
            )
          ]
    forM_ testSuite $ \(in', out) ->
      it (printf "translates %s to %s" (show in') (show out)) $ do
        desugar (S.Expr in') `shouldBe` Right out

  describe "Basic Bindings" $ do
    let
      testSuite =
        [ ( S.Let "x" [] (S.NumberLiteral 1)
          , C.Lam "_$1" (C.Bind ("x", C.Lit (C.Int 1)) (C.Var "_$1"))
          )
        , ( S.Let "x" ["a"] (S.Identifier "a")
          , C.Lam "_$1" (C.Bind ("x", C.Lam "a" (C.Var "a")) (C.Var "_$1"))
          )
        , ( S.Let "x" ["a", "b"] (S.Identifier "a")
          , C.Lam
            "_$1"
            (C.Bind ("x", C.Lam "a" (C.Lam "b" (C.Var "a"))) (C.Var "_$1"))
          )
        ]
    forM_ testSuite $ \(in', out) ->
      it (printf "translates %s to %s" (show in') (show out)) $ do
        desugar (S.Expr in') `shouldBe` Right out

  describe "Bindings in Blocks" $ do
    let testSuite =
          [ ( S.Block [S.Let "x" [] (S.NumberLiteral 1), S.Identifier "x"]
            , C.Bind ("x", C.Lit (C.Int 1)) (C.Var "x")
            )
          , ( S.Block
              [ S.Let "x" [] (S.NumberLiteral 1)
              , S.Let "y" [] (S.NumberLiteral 2)
              , S.BinOp "+" (S.Identifier "x") (S.Identifier "y")
              ]
            , C.Bind
              ("x", C.Lit (C.Int 1))
              (C.Bind ("y", C.Lit (C.Int 2))
                      (C.App (C.App (C.Var "+") (C.Var "x")) (C.Var "y"))
              )
            )
          , ( S.Block [S.Identifier "x", S.Identifier "y"]
            , C.Bind ("_$1", C.Var "x") (C.Var "y")
            )
          ]
    forM_ testSuite $ \(in', out) ->
      it (printf "translates %s to %s" (show in') (show out)) $ do
        desugar (S.Expr in') `shouldBe` Right out

  describe "Invalid blocks" $ do
    let testSuite =
          [ (S.Block [], "Unexpected empty block")
          , ( S.Block [S.Let "x" [] (S.NumberLiteral 1)]
            , "Illegal binding at the end of the block"
            )
          ]
    forM_ testSuite $ \(in', out) ->
      it (printf "translates %s to %s" (show in') (show out)) $ do
        desugar (S.Expr in') `shouldBe` Left out


  describe "Case expressions" $ do
    let
      testSuite =
        [ ( S.If (S.Identifier "x") (S.Identifier "y") (S.Identifier "z")
          , C.Case
            (C.Var "x")
            [ (C.LitP (C.Bool True) , C.Var "y")
            , (C.LitP (C.Bool False), C.Var "z")
            ]
          )
        , ( S.Match
            (S.Identifier "x")
            [ (S.NumberLiteral 1 , S.Identifier "y")
            , (S.BoolLiteral True, S.Identifier "z")
            ]
          , C.Case
            (C.Var "x")
            [(C.LitP (C.Int 1), C.Var "y"), (C.LitP (C.Bool True), C.Var "z")]
          )
        , ( S.Match
            (S.Identifier "x")
            [ (S.NumberLiteral 1, S.Identifier "y")
            , (S.Underscore     , S.Identifier "z")
            ]
          , C.Case (C.Var "x")
                   [(C.LitP (C.Int 1), C.Var "y"), (C.DefaultP, C.Var "z")]
          )
        , ( S.Match (S.Identifier "x") [(S.Identifier "y", S.Identifier "y")]
          , C.Case (C.Var "x") [(C.VarP "y", C.Var "y")]
          )
        , ( S.Match
            (S.Identifier "x")
            [(S.Tuple [S.NumberLiteral 1, S.NumberLiteral 2], S.Identifier "y")]
          , C.Case
            (C.Var "x")
            [(C.TupleP [C.LitP $ C.Int 1, C.LitP $ C.Int 2], (C.Var "y"))]
          )
        ]
    forM_ testSuite $ \(in', out) ->
      it (printf "translates %s to %s" (show in') (show out)) $ do
        desugar (S.Expr in') `shouldBe` Right out
