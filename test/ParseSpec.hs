{-# language QuasiQuotes #-}

module ParseSpec where

import           Control.Monad
import           Data.List
import           Data.String.Interpolate
import           Outputable
import           System.IO
import           System.IO.Silently
import           Test.Hspec

import           Helper
import           Parse

showAst :: Ast -> String
showAst = showSDocUnsafe . ppr

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses a simple module" $ do
      withFoo "foo = 3 -- bar" $ do
        ast <- parse ["Foo.hs"]
        fmap showAst ast `shouldBe` Right "[foo = 3]"

    it "handles an invalid module gracefully" $ do
      withFoo "foo = bar" $ do
        result <- parse ["Foo.hs"]
        void result `shouldBe` Left "Foo.hs:2:7: Not in scope: ‘bar’\n"

    it "doesn't output error messages" $ do
      withFoo "foo = bar" $ do
        output <- hCapture_ [stdout, stderr] $ parse ["Foo.hs"]
        output `shouldBe` ""

    it "doesn't follow imports" $ do
      let a = ("A", [i|
            module A where
            foo = ()
          |])
          b = ("B", [i|
            module B where
            import A
            bar = foo
          |])
      withModules [a, b] $ do
        ast <- parse ["B.hs"]
        fmap nameUsageGraph ast `shouldBe` Right [("B.bar", ["A.foo"])]

    it "can be used to parse multiple files" $ do
      let a = ("A", [i|
            module A where
            foo = foo
          |])
          b = ("B", [i|
            module B where
            bar = bar
          |])
      withModules [a, b] $ do
        ast <- parse ["A.hs", "B.hs"]
        fmap nameUsageGraph ast `shouldBe`
          Right [("A.foo", ["A.foo"]), ("B.bar", ["B.bar"])]

  describe "nameUsageGraph" $ do
    it "returns the graph of identifier usage" $ do
      withFoo [i|
        foo = bar
        bar = ()
      |] $ do
        Right ast <- parse ["Foo.hs"]
        sort (nameUsageGraph ast) `shouldBe`
          sort [("Foo.foo", ["Foo.bar"]), ("Foo.bar", ["GHC.Tuple.()"])]
