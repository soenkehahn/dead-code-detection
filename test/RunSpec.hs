{-# language QuasiQuotes #-}

module RunSpec where

import           Data.String.Interpolate
import           System.Environment
import           System.IO.Silently
import           Test.Hspec

import           Helper
import           Run

spec :: Spec
spec = do
  describe "run" $ do
    it "works" $ do
      let main = ("Main", [i|
            module Main where
            main = used
            used = return ()
            unused = ()
          |])
      withModules [main] $ do
        output <- capture_ $ withArgs ["Main.hs"] run
        output `shouldBe` "Main.unused\n"

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
