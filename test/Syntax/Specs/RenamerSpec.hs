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
        [ (BoolLiteral True , Right $ BoolLiteral True)
        , (BoolLiteral False, Right $ BoolLiteral False)
        , (NumberLiteral 1  , Right $ NumberLiteral 1)
        , (Identifier "x"   , Right $ Identifier "x")
        , ( Call (Identifier "f") [Identifier "a"]
          , Right $ Call (Identifier "f") [Identifier "a"]
          )
        , ( BinOp "+" (Identifier "x") (Identifier "y")
          , Right $ BinOp "+" (Identifier "x") (Identifier "y")
          )
        , ( Let "x" [] (NumberLiteral 1)
          , Right $ Let "x_$1" [] (NumberLiteral 1)
          )
        , ( Let "x" ["a"] (Identifier "a")
          , Right $ Let "x_$1" ["a_$1"] (Identifier "a_$1")
          )
        , ( Let "f" ["x"] (BinOp "+" (Identifier "x") (Identifier "x"))
          , Right $ Let "f_$1"
                        ["x_$1"]
                        (BinOp "+" (Identifier "x_$1") (Identifier "x_$1"))
          )
        , ( Let "f" ["x"] (Call (Identifier "x") [Identifier "y"])
          , Right
            $ Let "f_$1" ["x_$1"] (Call (Identifier "x_$1") [Identifier "y"])
          )
        , ( Let
            "f"
            ["x"]
            (If (BinOp "%" (Identifier "x") (NumberLiteral 0))
                (BoolLiteral True)
                (BoolLiteral False)
            )
          , Right $ Let
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
        rename in' `shouldBe` out

  describe "Block renaming" $ do
    let testSuite =
          [ ( Block
              [ Let "x" []         (NumberLiteral 1)
              , Let "y" [] (BinOp "+" (NumberLiteral 2) (Identifier "x"))
              , Let "x" ["y", "z"] (BinOp "+" (Identifier "y") (Identifier "z"))
              , Call (Identifier "x") [Identifier "y", NumberLiteral 2]
              ]
            , Right $ Block
              [ Let "x_$1" [] (NumberLiteral 1)
              , Let "y_$1" [] (BinOp "+" (NumberLiteral 2) (Identifier "x_$1"))
              , Let "x_$2"
                    ["y_$2", "z_$1"]
                    (BinOp "+" (Identifier "y_$2") (Identifier "z_$1"))
              , Call (Identifier "x_$2") [Identifier "y_$1", NumberLiteral 2]
              ]
            )
          ]
    forM_ testSuite $ \(in', out) ->
      it (printf "should rename %s to %s" (show in') (show out)) $ do
        rename in' `shouldBe` out

  describe "Conflicting definitions detections" $ do
    let testSuite =
          [ ( Let "f" ["f"] (Identifier "f")
            , Left "Conflicting definition for symbols 'f'"
            )
          , ( Let "f" ["a", "a"] (Identifier "f")
            , Left "Conflicting definition for symbols 'a'"
            )
          , ( Let "f" ["a", "a", "b", "c", "c"] (Identifier "f")
            , Left "Conflicting definition for symbols 'a', 'c'"
            )
          ]
    forM_ testSuite $ \(in', out) ->
      it (printf "should rename %s to %s" (show in') (show out)) $ do
        rename in' `shouldBe` out

