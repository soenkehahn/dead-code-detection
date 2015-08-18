{-# language QuasiQuotes #-}

module RunSpec where

import           Data.String.Interpolate
import           GHC
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
            module Main (main) where
            main = used
            used = return ()
            unused = ()
          |])
      withModules [main] $ do
        output <- capture_ $ withArgs (words "-i. --root Main") run
        output `shouldBe` "./Main.hs:4:1: unused\n"

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
        deadNamesFromFiles ["A.hs", "B.hs"] (mkModuleName "A")
          `shouldReturn` ["B.hs:2:1: bar"]

    it "only considers exported top-level declarations as roots" $ do
      let a = ("A", [i|
            module A (foo) where
            import B
            foo = ()
            bar = B.baz
          |])
          b = ("B", [i|
            module B where
            baz = ()
          |])
      withModules [a, b] $ do
        dead <- deadNamesFromFiles ["A.hs", "B.hs"] (mkModuleName "A")
        dead `shouldMatchList` ["A.hs:4:1: bar", "B.hs:2:1: baz"]
