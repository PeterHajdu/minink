module WebSpec(webSpec) where

import Test.Hspec

webSpec =
  describe "the web app" $ do
    it "should fail" $ do
      True `shouldBe` False
