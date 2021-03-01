module Flix.Specs.DesugarSpec
  ( spec
  ) where

import           Core                    hiding ( Expr )
import qualified Data.Map                      as Map
import           Data.Types
import           Flix.Desugar                   ( DesugarState(..)
                                                , desugar
                                                , makeEmptyState
                                                )
import           Flix.Syntax
import           Test.Hspec
import           Test.Util

spec :: Spec
spec = do
  describe "Atoms" $ do
    let testSuite =
          [ (BoolLiteral True   , Lit (Bool True))
          , (NumberLiteral 1    , Lit (Int 1))
          , (Identifier "x"     , Var "x")
          , (OperatorCapture "+", Var "+")
          , (Tuple []           , Lam "_$1" (Var "_$1"))
          , ( Tuple [Identifier "x", Identifier "y"]
            , Lam "_$1" (App (App (Var "_$1") (Var "x")) (Var "y"))
            )
          , (Lambda ["x"] (Identifier "x")     , Lam "x" (Var "x"))
          , (Lambda ["x", "y"] (Identifier "x"), Lam "x" (Lam "y" (Var "x")))
          ]
    forM_ testSuite $ \(in', out) ->
      it (printf "translates %s to %s" (show in') (show out)) $ do
        (fst <$> desugar makeEmptyState (Expr in')) `shouldBe` Right out

  describe "Calls" $ do
    let testSuite =
          [ (Call (Identifier "f") []              , Var "f")
          , (Call (Identifier "f") [Identifier "a"], App (Var "f") (Var "a"))
          , ( Call (Identifier "f") [Identifier "a", NumberLiteral 4]
            , App (App (Var "f") (Var "a")) (Lit $ Int 4)
            )
          , ( Call (Lambda ["x"] (Identifier "x")) [NumberLiteral 1]
            , App (Lam "x" (Var "x")) (Lit (Int 1))
            )
          , ( BinOp "+" (Identifier "x") (Identifier "y")
            , App (App (Var "+") (Var "x")) (Var "y")
            )
          ]
    forM_ testSuite $ \(in', out) ->
      it (printf "translates %s to %s" (show in') (show out)) $ do
        (fst <$> desugar makeEmptyState (Expr in')) `shouldBe` Right out

  describe "Basic Bindings" $ do
    let testSuite =
          [ ( Let "x" [] (NumberLiteral 1)
            , Lam "_$1" (Bind ("x", Lit (Int 1)) (Var "_$1"))
            )
          , ( LetMatch "x" [([], NumberLiteral 1)]
            , Lam "_$1" (Bind ("x", Lit (Int 1)) (Var "_$1"))
            )
          , ( Let "x" ["a"] (Identifier "a")
            , Lam "_$1" (Bind ("x", Lam "a" (Var "a")) (Var "_$1"))
            )
          , ( LetMatch "x" [([Identifier "a"], Identifier "a")]
            , Lam "_$1" (Bind ("x", Lam "a" (Var "a")) (Var "_$1"))
            )
          , ( Let "x" ["a", "b"] (Identifier "a")
            , Lam "_$1" (Bind ("x", Lam "a" (Lam "b" (Var "a"))) (Var "_$1"))
            )
          , ( LetMatch "x" [([Identifier "a", Identifier "b"], Identifier "a")]
            , Lam "_$1" (Bind ("x", Lam "a" (Lam "b" (Var "a"))) (Var "_$1"))
            )
          ]
    forM_ testSuite $ \(in', out) ->
      it (printf "translates %s to %s" (show in') (show out)) $ do
        (fst <$> desugar makeEmptyState (Expr in')) `shouldBe` Right out

  describe "Bindings in Blocks" $ do
    let
      testSuite =
        [ ( Block [Let "x" [] (NumberLiteral 1), Identifier "x"]
          , Bind ("x", Lit (Int 1)) (Var "x")
          )
        , ( Block
            [ Let "x" [] (NumberLiteral 1)
            , Let "y" [] (NumberLiteral 2)
            , BinOp "+" (Identifier "x") (Identifier "y")
            ]
          , Bind
            ("x", Lit (Int 1))
            (Bind ("y", Lit (Int 2)) (App (App (Var "+") (Var "x")) (Var "y")))
          )
        , ( Block [Identifier "x", Identifier "y"]
          , Bind ("_$1", Var "x") (Var "y")
          )
        ]
    forM_ testSuite $ \(in', out) ->
      it (printf "translates %s to %s" (show in') (show out)) $ do
        (fst <$> desugar makeEmptyState (Expr in')) `shouldBe` Right out

  describe "Invalid blocks" $ do
    let testSuite =
          [ (Block [], "Unexpected empty block")
          , ( Block [Let "x" [] (NumberLiteral 1)]
            , "Illegal binding at the end of the block"
            )
          ]
    forM_ testSuite $ \(in', out) ->
      it (printf "translates %s to %s" (show in') (show out)) $ do
        (fst <$> desugar makeEmptyState (Expr in')) `shouldBe` Left out


  describe "Case expressions" $ do
    let
      testSuite =
        [ ( If (Identifier "x") (Identifier "y") (Identifier "z")
          , Case (Var "x")
                 [(LitP (Bool True), Var "y"), (LitP (Bool False), Var "z")]
          )
        , ( Match
            (Identifier "x")
            [ (NumberLiteral 1 , Identifier "y")
            , (BoolLiteral True, Identifier "z")
            ]
          , Case (Var "x")
                 [(LitP (Int 1), Var "y"), (LitP (Bool True), Var "z")]
          )
        , ( Match
            (Identifier "x")
            [(NumberLiteral 1, Identifier "y"), (Underscore, Identifier "z")]
          , Case (Var "x") [(LitP (Int 1), Var "y"), (DefaultP, Var "z")]
          )
        , ( Match (Identifier "x") [(Identifier "y", Identifier "y")]
          , Case (Var "x") [(VarP "y", Var "y")]
          )
        , ( Match (Identifier "x")
                  [(Tuple [NumberLiteral 1, NumberLiteral 2], Identifier "y")]
          , Case (Var "x") [(TupleP [LitP $ Int 1, LitP $ Int 2], Var "y")]
          )
        ]
    forM_ testSuite $ \(in', out) ->
      it (printf "translates %s to %s" (show in') (show out)) $ do
        (fst <$> desugar makeEmptyState (Expr in')) `shouldBe` Right out

  describe "pattern matching let bindings" $ do
    let testSuites =
          [ ( LetMatch
              "l"
              [ ([NumberLiteral 1], NumberLiteral 1)
              , ([Underscore]     , NumberLiteral 2)
              ]
            , Lam
              "_$3"
              (Bind
                ( "l"
                , Lam
                  "_$1"
                  (Case
                    (Lam "_$2" (App (Var "_$2") (Var "_$1")))
                    [ (TupleP [LitP (Int 1)], Lit (Int 1))
                    , (TupleP [DefaultP]    , Lit (Int 2))
                    ]
                  )
                )
                (Var "_$3")
              )
            )
          ]
    forM_ testSuites $ \(in', out) ->
      it (printf "translated %s" (show in')) $ do
        (fst <$> desugar makeEmptyState (Expr in')) `shouldBe` Right out

  describe "Constructor pattern matching" $ do
    it "creates a constructor pattern match" $ do
      let input = Expr $ Match
            (Identifier "xs")
            [ ( Call (Identifier ":") [Identifier "x", Underscore]
              , BoolLiteral True
              )
            ]
      let output = Case
            (Var "xs")
            [ ( ConstrP (AnyTy :~> AnyTy :~> NominalTy ":") [VarP "x", DefaultP]
              , Lit $ Bool True
              )
            ]
      let tys = Map.fromList [(":", AnyTy :~> AnyTy :~> NominalTy ":")]
      desugar (DesugarState 0 tys) input `shouldBe` Right (output, tys)

    it "errors on unknown constructor" $ do
      let input = Expr $ Match
            (Identifier "xs")
            [ ( Call (Identifier ":") [Identifier "x", Underscore]
              , BoolLiteral True
              )
            ]
      let output = Case
            (Var "xs")
            [ ( ConstrP (AnyTy :~> AnyTy :~> NominalTy ":") [VarP "x", DefaultP]
              , Lit $ Bool True
              )
            ]
      desugar makeEmptyState input `shouldBe` Left "Constructor ':' not found"



  describe "Declarations" $ do
    let
      tesSuite =
        [ ( Module "TestModule"
                   [Expr $ Let "x" [] (Identifier "x"), Expr $ Identifier "x"]
          , Bind ("x", Var "x") (Var "x")
          , mempty
          )
        , ( Module "TestModule" [Decl $ Record "Nil" [], Expr $ Identifier "x"]
          , Bind ("Nil", Lam "_$1" (Var "_$1")) (Var "x")
          , Map.fromList [("Nil", NominalTy "Nil")]
          )
        , ( Module
            "TestModule"
            [Decl $ Record "Cons" ["head", "tail"], Expr $ Identifier "x"]
          , Bind
            ( "Cons"
            , Lam
              "head"
              (Lam
                "tail"
                (Lam "_$3" (App (App (Var "_$3") (Var "head")) (Var "tail")))
              )
            )
            (Bind
              ( "head"
              , Lam "_$1"
                    (App (Var "_$1") (Lam "head" (Lam "tail" (Var "head"))))
              )
              (Bind
                ( "tail"
                , Lam "_$2"
                      (App (Var "_$2") (Lam "head" (Lam "tail" (Var "tail"))))
                )
                (Var "x")
              )
            )
          , Map.fromList [("Cons", AnyTy :~> AnyTy :~> NominalTy "Cons")]
          )
        ]
    forM_ tesSuite $ \(in', out, ty) ->
      it (printf "translates declaration %s" (show in')) $ do
        desugar makeEmptyState (Decl in') `shouldBe` Right (out, ty)
