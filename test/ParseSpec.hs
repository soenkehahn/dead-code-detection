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

spec = do
  describe "parse" $ do
    it "parses a simple module" $ do
      inTempDirectory $ do
        writeFile "Foo.hs" $ unindent [i|
          module Foo where
          foo = 3 -- bar
        |]
        ast <- parse "Foo.hs"
        fmap showAst ast `shouldBe` Right "foo = 3"

    it "handles an invalid module gracefully" $ do
      inTempDirectory $ do
        writeFile "Foo.hs" $ unindent [i|
          module Foo where
          foo = bar
        |]
        result <- parse "Foo.hs"
        void result `shouldBe` Left "Foo.hs:2:7: Not in scope: ‘bar’\n"

  describe "nameUsageGraph" $ do
    it "returns the graph of identifier usage" $ do
      inTempDirectory $ do
        writeFile "Foo.hs" $ unindent [i|
          module Foo where
          foo = bar
          bar = ()
        |]
        Right graph <- parse "Foo.hs"
        sort (nameUsageGraph graph) `shouldBe` sort [("foo", ["bar"]), ("bar", ["()"])]

    it "returns qualified names" $ do
      pending

showAst :: Ast -> String
showAst = showSDocUnsafe . ppr
