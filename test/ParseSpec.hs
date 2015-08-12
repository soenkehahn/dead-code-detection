{-# language QuasiQuotes #-}

module ParseSpec where

import           Control.Exception
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
        showAst ast `shouldBe` "foo = 3"

    it "handles an invalid module gracefully" $ do
      inTempDirectory $ do
        writeFile "Foo.hs" $ unindent [i|
          module Foo where
          foo = bar
        |]
        parse "Foo.hs" `shouldThrow` (== ErrorCall "not in scope: bar")

showAst :: Ast -> String
showAst = showSDocUnsafe . ppr
