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
        Right ast <- parse ["Foo.hs"]
        let graph = nameUsageGraph ast
            Right root = findName graph "Foo.foo"
        fmap showName (deadNames graph root)
          `shouldBe` ["Foo.bar"]
