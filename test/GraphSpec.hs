{-# language QuasiQuotes #-}

module GraphSpec where

import           Data.String.Interpolate
import           Test.Hspec

import           Graph
import           Helper
import           Parse

spec :: Spec
spec = do
  describe "deadNames" $ do
    it "detects unused top-level names" $ do
      withFoo [i|
        foo = ()
        bar = ()
      |] $ do
        Right graph <- parse ["Foo.hs"]
        fmap showName (deadNames (nameUsageGraph graph) "Foo.foo")
          `shouldBe` ["Foo.bar"]
