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
        fmap showName (deadNames graph [root])
          `shouldBe` ["Foo.bar"]

    it "allows to specify multiple roots" $ do
      withFoo [i|
        r1 = foo
        r2 = bar
        foo = ()
        bar = ()
        baz = ()
      |] $ do
        Right ast <- parse ["Foo.hs"]
        let graph = nameUsageGraph ast
            Right roots = mapM (findName graph) ["Foo.r1", "Foo.r2"]
        fmap showName (deadNames graph roots)
          `shouldBe` ["Foo.baz"]
