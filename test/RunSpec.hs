{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RunSpec where

import           Control.Exception
import           Data.String.Interpolate
import           GHC
import           System.Environment
import           System.Exit
import           System.IO.Silently
import           Test.Hspec

import           Helper
import           Run

spec :: Spec
spec = do
  describe "run" $ do
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
          output <- capture_ $
            handle (\ (_ :: SomeException) -> return ()) run'
          output `shouldBe` "./Main.hs:4:1: unused\n"

      it "exits with a non-zero exit-code" $ do
        withModules [main] $ do
          run' `shouldThrow` (== ExitFailure 1)

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
