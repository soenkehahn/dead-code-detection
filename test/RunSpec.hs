{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RunSpec where

import           Control.Exception
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
          run' `shouldDie` ""

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

    it "has a version option" $ do
      output <- capture_ $
        handle (\ ExitSuccess -> return ()) $
        withArgs ["--version"] run
      output `shouldContain` "version: "

    context "--ignore" $ do

      it "ignores files if told to do so" $ do
        let main = ("Main", [i|
              module Main where
              main = return ()
            |])
            b = ("B", [i|
              This is some arbitrary text that is not Haskell.
              |])
            run' = withArgs (words "-i. -e./B.hs --root Main") run
        withModules [main, b] $ run' `shouldReturn` ()

      it "errors out on missing ignored files" $ do
        let main = ("Main", [i|
              module Main where
              main = return ()
            |])
            run' = withArgs (words "-i. -e./B.hs --root Main") run
        withModules [main] $ run' `shouldDie` "file not found: ./B.hs\n"

      it "ignores files if referenced differently" $ do
        let main = ("Main", [i|
              module Main where
              main = return ()
            |])
            b = ("B", [i|
              This is some arbitrary text that is not Haskell.
              |])
            run' = withArgs (words "-i. -e B.hs --root Main") run
        withModules [main, b] $ run' `shouldReturn` ()

  describe "deadNamesFromFiles" $ do
    it "should clearly mark ghc's output as such" $ do
      let a = ("A", [i|
            module A where
            import B
          |])
      withModules [a] $ do
        output <- hCapture_ [stderr] $
          deadNamesFromFiles ["A.hs"] [mkModuleName "A"] False
            `shouldThrow` (== ExitFailure 1)
        output `shouldContain` "ghc says:"

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
        deadNamesFromFiles ["A.hs", "B.hs"] [mkModuleName "A"] False
          `shouldReturn` ["B.hs:2:1: bar"]

    context "names starting with an underscore" $ do
      it "excludes them by default" $ do
        let a = ("A", [i|
              module A (foo) where
              foo = ()
              _bar = ()
            |])
        withModules [a] $ do
          dead <- deadNamesFromFiles ["A.hs"] [mkModuleName "A"] False
          dead `shouldMatchList` []

      it "includes them if asked to" $ do
        let a = ("A", [i|
              module A (foo) where
              foo = ()
              _bar = ()
            |])
        withModules [a] $ do
          dead <- deadNamesFromFiles ["A.hs"] [mkModuleName "A"] True
          dead `shouldMatchList` ["A.hs:3:1: _bar"]

    it "excludes constructor names" $ do
        let a = ("A", [i|
              module A () where
              data A = A
            |])
        withModules [a] $ do
          dead <- deadNamesFromFiles ["A.hs"] [mkModuleName "A"] True
          dead `shouldMatchList` []

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
        dead <- deadNamesFromFiles ["A.hs", "B.hs"] [mkModuleName "A"] False
        dead `shouldMatchList` ["A.hs:4:1: bar", "B.hs:2:1: baz"]

shouldDie :: IO a -> String -> IO ()
shouldDie action err = do
  (output, exception) <- hCapture [stderr] $ catch
    (action >> return Nothing)
    (\ (e :: ExitCode) -> return $ Just e)
  case exception of
    Nothing -> throwIO $ ErrorCall "shouldDie: didn't receive ExitCode exception"
    Just ExitSuccess -> throwIO $ ErrorCall "shouldDie: received ExitSuccess exception"
    Just (ExitFailure _) ->
      output `shouldBe` err
