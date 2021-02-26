module Syntax.Specs.RenamerSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import           Syntax.Core
import           Syntax.Renamer                 ( rename )
import           Test.Hspec
import           Text.Printf                    ( printf )

spec :: Spec
spec = do
  describe "Top level expressions" $ do
    let
      testSuite =
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
        , (Let "x" [] (NumberLiteral 1), Let "x_$1" [] (NumberLiteral 1))
        , ( Let "x"    ["a"]    (Identifier "a")
          , Let "x_$1" ["a_$1"] (Identifier "a_$1")
          )
        , ( Let "f"    ["f"]    (Identifier "f")
          , Let "f_$1" ["f_$2"] (Identifier "f_$2")
          )
        , ( Let "f" ["x"] (BinOp "+" (Identifier "x") (Identifier "x"))
          , Let "f_$1"
                ["x_$1"]
                (BinOp "+" (Identifier "x_$1") (Identifier "x_$1"))
          )
        , ( Let "f"    ["x"]    (Call (Identifier "x") [Identifier "y"])
          , Let "f_$1" ["x_$1"] (Call (Identifier "x_$1") [Identifier "y"])
          )
        , ( Let
            "f"
            ["x"]
            (If (BinOp "%" (Identifier "x") (NumberLiteral 0))
                (BoolLiteral True)
                (BoolLiteral False)
            )
          , Let
            "f_$1"
            ["x_$1"]
            (If (BinOp "%" (Identifier "x_$1") (NumberLiteral 0))
                (BoolLiteral True)
                (BoolLiteral False)
            )
          )
        ]
    forM_ testSuite $ \(in', out) ->
      it (printf "should rename %s to %s" (show in') (show out)) $ do
        rename (Expr in') `shouldBe` Right (Expr out)

  describe "Block renaming" $ do
    let
      testSuite =
        [ ( Block
            [ Let "x" []         (NumberLiteral 1)
            , Let "y" []         (BinOp "+" (NumberLiteral 2) (Identifier "x"))
            , Let "x" ["y", "z"] (BinOp "+" (Identifier "y") (Identifier "z"))
            , Call (Identifier "x") [Identifier "y", NumberLiteral 2]
            ]
          , Block
            [ Let "x_$1" [] (NumberLiteral 1)
            , Let "y_$1" [] (BinOp "+" (NumberLiteral 2) (Identifier "x_$1"))
            , Let "x_$2"
                  ["y_$2", "z_$1"]
                  (BinOp "+" (Identifier "y_$2") (Identifier "z_$1"))
            , Call (Identifier "x_$2") [Identifier "y_$1", NumberLiteral 2]
            ]
          )
        , ( Block
            [ Let "x" [] (NumberLiteral 1)
            , Tuple [Identifier "x", Identifier "x"]
            ]
          , Block
            [ Let "x_$1" [] (NumberLiteral 1)
            , Tuple [Identifier "x_$1", Identifier "x_$1"]
            ]
          )
        ]
    forM_ testSuite $ \(in', out) ->
      it (printf "should rename %s to %s" (show in') (show out)) $ do
        rename (Expr in') `shouldBe` Right (Expr out)

  describe "Lambda renaming" $ do
    it "renames arguments" $ do
      let in' = Lambda ["x"] (Identifier "x")
      let out = Lambda ["x_$1"] (Identifier "x_$1")
      rename (Expr in') `shouldBe` Right (Expr out)

  -- describe "Match expressions" $ do
  --   it "renames branch variables" $ do
  --     let
  --       in' =
  --         Let "f" ["x"] (Match (Identifier "x") [(Underscore, Identifier "x")])
  --     let out = Let
  --           "f_$1"
  --           ["x_$1"]
  --           (Match (Identifier "x_$1") [(Underscore, Identifier "x_$1")])
  --     rename (Expr in') `shouldBe` Right (Expr out)
  --   it "introduce new names on variable capture" $ do
  --     let
  --       in' = Let "f"
  --                 ["x"]
  --                 (Match (Identifier "x") [(Identifier "y", Identifier "y")])
  --     let out = Let
  --           "f_$1"
  --           ["x_$1"]
  --           (Match (Identifier "x_$1") [(Identifier "y_$1", Identifier "y_$1")])
  --     rename (Expr in') `shouldBe` Right (Expr out)

  describe "Conflicting definitions detections" $ do
    let testSuite =
          [ ( Let "f" ["a", "a"] (Identifier "f")
            , Left "Conflicting definition for symbols 'a'"
            )
          , ( Let "f" ["a", "a", "b", "c", "c"] (Identifier "f")
            , Left "Conflicting definition for symbols 'a', 'c'"
            )
          ]
    forM_ testSuite $ \(in', out) ->
      it (printf "rename %s to %s" (show in') (show out)) $ do
        rename (Expr in') `shouldBe` out

  describe "Declarations" $ do
    let
      testSuite =
        [ (Module "TestModule" [], Module "TestModule" [])
        , ( Module "TestModule" [Expr $ Let "x" [] (NumberLiteral 1)]
          , Module "TestModule"
                   [Expr $ Let "TestModule.x_$1" [] (NumberLiteral 1)]
          )
        , (Record "Nil" [], Record "Nil_$1" [])
        , ( Record "Cons"    ["head", "tail"]
          , Record "Cons_$1" ["head_$1", "tail_$1"]
          )
        , ( Module
            "TestModule"
            [ Decl $ Record "Cons" ["head", "tail"]
            , Expr $ Let "head" [] (NumberLiteral 1)
            ]
          , Module
            "TestModule"
            [ Decl $ Record "TestModule.Cons_$1"
                            ["TestModule.head_$1", "TestModule.tail_$1"]
            , Expr $ Let "TestModule.head_$2" [] (NumberLiteral 1)
            ]
          )
        ]
    forM_ testSuite $ \(in', out) ->
      it (printf "renamed %s to %s" (show in') (show out)) $ do
        rename (Decl in') `shouldBe` Right (Decl out)

