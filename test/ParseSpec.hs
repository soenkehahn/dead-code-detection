{-# language QuasiQuotes #-}

module ParseSpec where

import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           Test.Hspec
import           Test.Mockery.Directory

import           Parse

spec = do
  describe "readAst" $ do
    it "fixme" $ do
      inTempDirectory $ do
        writeFile "Test.hs" $ unindent [i|
          module Foo where
        |]
        ast <- parse "Test.hs"
        pending
