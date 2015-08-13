{-# language QuasiQuotes #-}

module ParseSpec where

import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           Outputable
import           Test.Hspec
import           Test.Mockery.Directory

import           Parse

showAst :: Ast -> String
showAst = showSDocUnsafe . ppr

withFoo :: String -> IO () -> IO ()
withFoo code action = do
  inTempDirectory $ do
    writeFile "Foo.hs" $
      "module Foo where\n" ++
      unindent code
    action

spec = do
  describe "parse" $ do
    it "parses a simple module" $ do
      withFoo "foo = 3 -- bar" $ do
        ast <- parse "Foo.hs"
        fmap showAst ast `shouldBe` Right "foo = 3"

    it "handles an invalid module gracefully" $ do
      withFoo "foo = bar" $ do
        result <- parse "Foo.hs"
        void result `shouldBe` Left "Foo.hs:2:7: Not in scope: ‘bar’\n"

  describe "nameUsageGraph" $ do
    it "returns the graph of identifier usage" $ do
      withFoo [i|
        foo = bar
        bar = ()
      |] $ do
        Right graph <- parse "Foo.hs"
        sort (nameUsageGraph graph) `shouldBe` sort [("Foo.foo", ["Foo.bar"]), ("Foo.bar", ["GHC.Tuple.()"])]
