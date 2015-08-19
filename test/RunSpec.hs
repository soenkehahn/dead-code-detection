{-# LANGUAGE QuasiQuotes #-}

module RunSpec where

import           Data.String.Interpolate
import           GHC
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Silently
import           Test.Hspec

import           Helper
import           Run

spec :: Spec
spec = do
  describe "run" $ around_ (hSilence [stdout, stderr]) $ do
    context "when given a module containing dead code" $ do
      let main = ("Main", [i|
            module Main (main) where
            main = used
            used = return ()
            unused = ()
          |])
          run' = withArgs (words "-i. --root Main") run
      it "works" $ do
        withModules [main] $ do
          output <- capture_ $ swallowExceptions run'
          output `shouldBe` "./Main.hs:4:1: unused\n"

      it "exits with a non-zero exit-code" $ do
        withModules [main] $ do
          run' `shouldThrow` (== ExitFailure 1)

    it "allows to set multiple roots" $ do
      let a = ("A", [i|
            module A (a) where
            a = ()
          |])
          b = ("B", [i|
            module B (b) where
            b = ()
          |])
      withModules [a, b] $ do
        output <- hCapture_ [stdout, stderr] $ swallowExceptions $
          withArgs (words "-i. --root A --root B") run
        output `shouldBe` ""

    it "complains when it's invoked with no arguments" $ do
      withArgs [] run `shouldThrow` (== ExitFailure 1)

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
        deadNamesFromFiles ["A.hs", "B.hs"] [mkModuleName "A"]
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
        dead <- deadNamesFromFiles ["A.hs", "B.hs"] [mkModuleName "A"]
        dead `shouldMatchList` ["A.hs:4:1: bar", "B.hs:2:1: baz"]
