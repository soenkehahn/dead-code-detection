{-# language QuasiQuotes #-}

module ParseSpec where

import           Control.Monad
import           Data.String.Interpolate
import           GHC
import           Outputable
import           System.IO
import           System.IO.Silently
import           Test.Hspec

import           GHC.Show
import           Graph
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
        fmap showAst ast `shouldBe` Right "[Module Foo Nothing [foo = 3]]"

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
        parseStringGraph ["B.hs"] `shouldReturn`
          Graph [("B.bar", ["A.foo"])]

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
        parseStringGraph ["A.hs", "B.hs"] `shouldReturn`
          Graph [("A.foo", ["A.foo"]), ("B.bar", ["B.bar"])]

  describe "findExports" $ do
    let find moduleFile moduleName = do
          Right ast <- parse [moduleFile]
          let Right exports = findExports ast moduleName
          return $ map showName exports
    it "finds the names exported by a given module" $ do
      withFoo [i|
        foo = ()
        bar = ()
      |] $ do
        exports <- find "Foo.hs" (mkModuleName "Foo")
        exports `shouldMatchList` ["Foo.foo", "Foo.bar"]

    context "when given a module with an export list" $ do
      it "returns the explicit exports" $ do
        let a = ("A", [i|
              module A (foo) where
              foo = ()
              bar = ()
            |])
        withModules [a] $ do
          exports <- find "A.hs" (mkModuleName "A")
          exports `shouldBe` ["A.foo"]

  describe "nameUsageGraph" $ do
    it "returns the graph of identifier usage" $ do
      withFoo [i|
        foo = bar
        bar = ()
      |] $ do
        parseStringGraph ["Foo.hs"] `shouldReturn`
          Graph [("Foo.foo", ["Foo.bar"]), ("Foo.bar", ["GHC.Tuple.()"])]
