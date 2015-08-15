{-# language QuasiQuotes #-}

module GraphSpec where

import           Data.String.Interpolate
import           Test.Hspec

import           Graph
import           Helper

spec :: Spec
spec = do
  describe "deadNames" $ do
    it "detects unused top-level names" $ do
      withFoo [i|
        foo = ()
        bar = ()
      |] $ do
        graph <- parseStringGraph ["Foo.hs"]
        deadNames graph "Foo.foo" `shouldBe` ["Foo.bar"]
