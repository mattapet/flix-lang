module Flix.Specs.RenamerSpec
  ( spec
  ) where

import           Flix.FlixMonad                 ( runFlixMonad )
import           Flix.FlixState                 ( makeEmptyState )
import           Flix.Renamer                   ( rename )
import           Flix.Syntax
import           Test.Hspec
import           Test.Util

spec :: Spec
spec = do
  describe "Top level expressions" $ do
    let testSuite =
          [ (Underscore         , Underscore)
          , (BoolLiteral True   , BoolLiteral True)
          , (BoolLiteral False  , BoolLiteral False)
          , (NumberLiteral 1    , NumberLiteral 1)
          , (Identifier "x"     , Identifier "x")
          , (OperatorCapture "+", OperatorCapture "+")
          , (Tuple []           , Tuple [])
          , ( Tuple [NumberLiteral 1, NumberLiteral 2]
            , Tuple [NumberLiteral 1, NumberLiteral 2]
            )
          , ( Call (Identifier "f") [Identifier "a"]
            , Call (Identifier "f") [Identifier "a"]
            )
          , ( BinOp "+" (Identifier "x") (Identifier "y")
            , BinOp "+" (Identifier "x") (Identifier "y")
            )
          , ( Let (Identifier "x")    (NumberLiteral 1)
            , Let (Identifier "x_$1") (NumberLiteral 1)
            )
          , ( Let (Tuple [Identifier "x", Identifier "y"])
                  (Tuple [Identifier "y", Identifier "x"])
            , Let (Tuple [Identifier "x_$1", Identifier "y_$1"])
                  (Tuple [Identifier "y", Identifier "x"])
            )
          ]
    forM_ testSuite $ \(in', out) ->
      it (printf "should rename %s to %s" (show in') (show out)) $ do
        runRenamer (Expr in') `shouldBe` Right (Expr out)

  describe "Block renaming" $ do
    let
      testSuite =
        [ ( Block
            [ Let (Identifier "x") (NumberLiteral 1)
            , Let (Identifier "y")
                  (BinOp "+" (NumberLiteral 2) (Identifier "x"))
            , Let (Identifier "x") (BinOp "+" (Identifier "x") (Identifier "y"))
            , Identifier "x"
            ]
          , Block
            [ Let (Identifier "x_$1") (NumberLiteral 1)
            , Let (Identifier "y_$1")
                  (BinOp "+" (NumberLiteral 2) (Identifier "x_$1"))
            , Let (Identifier "x_$2")
                  (BinOp "+" (Identifier "x_$1") (Identifier "y_$1"))
            , Identifier "x_$2"
            ]
          )
        , ( Block
            [ Let (Identifier "x") (NumberLiteral 1)
            , Tuple [Identifier "x", Identifier "x"]
            ]
          , Block
            [ Let (Identifier "x_$1") (NumberLiteral 1)
            , Tuple [Identifier "x_$1", Identifier "x_$1"]
            ]
          )
        ]
    forM_ testSuite $ \(in', out) ->
      it (printf "should rename %s to %s" (show in') (show out)) $ do
        runRenamer (Expr in') `shouldBe` Right (Expr out)

  describe "Lambda renaming" $ do
    it "renames arguments" $ do
      let in' = Lambda ["x"] (Identifier "x")
      let out = Lambda ["x_$1"] (Identifier "x_$1")
      runRenamer (Expr in') `shouldBe` Right (Expr out)

  describe "Match expressions" $ do
    it "renames branch variables" $ do
      let in' = Let (Identifier "f")
                    (Match (Identifier "x") [(Underscore, Identifier "x")])
      let out = Let (Identifier "f_$1")
                    (Match (Identifier "x") [(Underscore, Identifier "x")])
      runRenamer (Expr in') `shouldBe` Right (Expr out)

    it "introduce new names on variable capture" $ do
      let in' = Let
            (Identifier "f")
            (Match (Identifier "x") [(Identifier "y", Identifier "y")])
      let out = Let
            (Identifier "f_$1")
            (Match (Identifier "x") [(Identifier "y_$1", Identifier "y_$1")])
      runRenamer (Expr in') `shouldBe` Right (Expr out)

    it "introduce new names on variable in tuple pattern capture" $ do
      let in' = Let
            (Identifier "f")
            (Match (Identifier "x")
                   [(Tuple [Identifier "h", Identifier "t"], Identifier "h")]
            )
      let out = Let
            (Identifier "f_$1")
            (Match
              (Identifier "x")
              [ ( Tuple [Identifier "h_$1", Identifier "t_$1"]
                , Identifier "h_$1"
                )
              ]
            )
      runRenamer (Expr in') `shouldBe` Right (Expr out)

  describe "Operator def renaming" $ do
    it "renamed defined def operator" $ do
      let input = Block
            [ Def
              "==="
              [ ( [Identifier "lhs", Identifier "rhs"]
                , Call (Identifier "equals")
                       [Identifier "lhs", Identifier "rhs"]
                )
              ]
            , BinOp "===" (Identifier "x") (Identifier "y")
            ]
      let output = Block
            [ Def
              "===_$1"
              [ ( [Identifier "lhs_$1", Identifier "rhs_$1"]
                , Call (Identifier "equals")
                       [Identifier "lhs_$1", Identifier "rhs_$1"]
                )
              ]
            , BinOp "===_$1" (Identifier "x") (Identifier "y")
            ]
      runRenamer (Expr input) `shouldBe` Right (Expr output)


  describe "Pattern matching def bindings" $ do
    let
      testSuite =
        [ ( Block [Def "x" [([], NumberLiteral 1)]]
          , Block [Def "x_$1" [([], NumberLiteral 1)]]
          )
        , ( Block
            [ Def "x"
                  [([Tuple [Identifier "x", Identifier "y"]], Identifier "x")]
            ]
          , Block
            [ Def
                "x_$1"
                [ ( [Tuple [Identifier "x_$2", Identifier "y_$1"]]
                  , Identifier "x_$2"
                  )
                ]
            ]
          )
        , ( Block
            [ Def "x" [([], Call (Identifier "y") [NumberLiteral 2])]
            , Def
              "y"
              [ ( [Tuple [Identifier "y"]]
                , BinOp "+" (NumberLiteral 1) (Identifier "y")
                )
              ]
            , Identifier "x"
            ]
          , Block
            [ Def "x_$1" [([], Call (Identifier "y_$1") [NumberLiteral 2])]
            , Def
              "y_$1"
              [ ( [Tuple [Identifier "y_$2"]]
                , BinOp "+" (NumberLiteral 1) (Identifier "y_$2")
                )
              ]
            , Identifier "x_$1"
            ]
          )
        ]
    forM_ testSuite $ \(in', out) -> it (printf "renames %s" (show in')) $ do
      runRenamer (Expr in') `shouldBe` Right (Expr out)

  describe "Conflicting definitions detections" $ do
    let testSuite =
          [ ( Def "f" [([Identifier "a", Identifier "a"], Identifier "f")]
            , Left "Conflicting definition for symbols 'a'"
            )
          , ( Def
              "f"
              [ ( [ Identifier "a"
                  , Identifier "a"
                  , Identifier "b"
                  , Identifier "c"
                  , Identifier "c"
                  ]
                , Identifier "f"
                )
              ]
            , Left "Conflicting definition for symbols 'a', 'c'"
            )
          ]
    forM_ testSuite $ \(in', out) ->
      it (printf "rename %s to %s" (show in') (show out)) $ do
        runRenamer (Expr in') `shouldBe` out

  describe "Declarations" $ do
    let
      testSuite =
        [ (Module "TestModule" [], Module "TestModule" [])
        , ( Module "TestModule" [Expr $ Let (Identifier "x") (NumberLiteral 1)]
          , Module
            "TestModule"
            [Expr $ Let (Identifier "TestModule.x_$1") (NumberLiteral 1)]
          )
        , (Record "Nil" [], Record "Nil_$1" [])
        , ( Record "Cons"    ["head", "tail"]
          , Record "Cons_$1" ["head_$1", "tail_$1"]
          )
        , ( Module
            "TestModule"
            [ Decl $ Record "Cons" ["head", "tail"]
            , Expr $ Let (Identifier "head") (Identifier "head")
            ]
          , Module
            "TestModule"
            [ Decl $ Record "TestModule.Cons_$1"
                            ["TestModule.head_$1", "TestModule.tail_$1"]
            , Expr $ Let (Identifier "TestModule.head_$2")
                         (Identifier "TestModule.head_$1")
            ]
          )
        ]
    forM_ testSuite $ \(in', out) ->
      it (printf "renamed %s to %s" (show in') (show out)) $ do
        runRenamer (Decl in') `shouldBe` Right (Decl out)

    it "fails upon module re-declaration" $ do
      let input = Decl $ Module "M1" [Decl $ Module "M2" []]
      runRenamer input
        `shouldBe` Left "Unexpected module re-declaration of module 'M1'"


runRenamer :: AST -> Either String AST
runRenamer input = fst <$> runFlixMonad (rename input) makeEmptyState
