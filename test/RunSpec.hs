{-# language QuasiQuotes #-}

module RunSpec where

import           Data.String.Interpolate
import           Test.Hspec

import           Helper
import           Run

spec :: Spec
spec = do
  describe "deadNamesFromFiles" $ do
    it "can be run on multiple modules" $ do
      let a = ("A", [i|
            module A where
            foo = ()
          |])
          b = ("B", [i|
            module B where
            bar = ()
          |])
      withModules [a, b] $ do
        deadNamesFromFiles ["A.hs", "B.hs"] "A.foo"
          `shouldReturn` ["B.bar"]

    it "includes source locations" $ do
      pending
