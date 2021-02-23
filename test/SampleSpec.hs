module SampleSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "sample test" $ do
    it "should do nothing" $ do
      1 `shouldBe` 1
