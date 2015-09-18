{-# language QuasiQuotes #-}

module AstSpec where

import           Control.Monad
import           Data.String.Interpolate
import           GHC
import           Outputable
import           System.Directory
import           System.IO
import           System.IO.Silently
import           Test.Hspec

import           Ast
import           Graph
import           Helper

showAst :: Ast -> String
showAst = showSDocUnsafe . ppr

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses a simple module" $ do
      withFooHeader "foo = 3 -- bar" $ do
        ast <- parse ["Foo.hs"]
        fmap showAst ast `shouldBe` Right "[Module Foo Nothing [foo = 3]]"

    it "handles an invalid module gracefully" $ do
      withFooHeader "foo = bar" $ do
        result <- parse ["Foo.hs"]
        void result `shouldBe` Left "\nFoo.hs:2:7: Not in scope: ‘bar’\n"

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
          Graph [("B.bar", ["A.foo"])] []

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
          Graph [("A.foo", ["A.foo"]), ("B.bar", ["B.bar"])] []

    it "does not create any files" $ do
      withFooHeader [i|
        foo = ()
        bar = ()
      |] $ do
        _ <- parse ["Foo.hs"]
        files <- getDirectoryContents "."
        files `shouldMatchList` (words ". .. Foo.hs")

  describe "findExports" $ do
    let find moduleFiles moduleName = do
          ast <- either error id <$> parse moduleFiles
          let exports = either error id $
                findExports ast moduleName
          return $ map showName exports
    it "finds the names exported by a given module" $ do
      withFooHeader [i|
        foo = ()
        bar = ()
      |] $ do
        exports <- find ["Foo.hs"] [mkModuleName "Foo"]
        exports `shouldMatchList` ["Foo.foo", "Foo.bar"]

    context "when given a module with an export list" $ do
      it "returns the explicit exports" $ do
        let a = ("A", [i|
              module A (foo) where
              foo = ()
              bar = ()
            |])
        withModules [a] $ do
          exports <- find ["A.hs"] [mkModuleName "A"]
          exports `shouldBe` ["A.foo"]

    it "includes identifiers exported by module" $ do
      let a = ("A", [i|
            module A (module B) where
            import B
          |])
          b = ("B", [i|
            module B where
            b = ()
          |])
      withModules [a, b] $ do
        exports <- find ["A.hs", "B.hs"] [mkModuleName "A"]
        exports `shouldBe` ["B.b"]

  describe "usedTopLevelNames" $ do
    it "returns the graph of identifier usage" $ do
      withFooHeader [i|
        foo = bar
        bar = ()
      |] $ do
        g <- usageGraph <$> parseStringGraph ["Foo.hs"]
        g `shouldMatchList` [("Foo.foo", ["Foo.bar"]), ("Foo.bar", ["GHC.Tuple.()"])]

    it "detects usage in ViewPatterns" $ do
      withFoo [i|
        {-# LANGUAGE ViewPatterns #-}
        module Foo where
        x y = ()
        bar (x -> y) = ()
      |] $ do
        g <- usageGraph <$> parseStringGraph ["Foo.hs"]
        let Just used = lookup "Foo.bar" g
        used `shouldContain` ["Foo.x"]

    it "doesn't return local variables" $ do
      withFooHeader [i|
        foo = let x = x in x
      |] $ do
        parseStringGraph ["Foo.hs"] `shouldReturn`
          Graph [("Foo.foo", [])] []

    context "data type declarations" $ do
      it "does not include constructor names" $ do
        withFooHeader [i|
          data A = A
        |] $ do
          parseStringGraph ["Foo.hs"] `shouldReturn`
            Graph [] []

      it "ignores selectors" $ do
        withFooHeader [i|
          data A = A { foo :: () }
        |] $ do
          boundNames <- map fst <$> usageGraph <$> parseStringGraph ["Foo.hs"]
          boundNames `shouldBe` []

      it "does not detect selectors starting with _" $ do
        withFooHeader [i|
          data A = A { _foo :: () }
        |] $ do
          boundNames <- map fst <$> usageGraph <$> parseStringGraph ["Foo.hs"]
          boundNames `shouldNotContain` ["Foo._foo"]

    it "doesn't return bound names for instance methods" $ do
      withFooHeader [i|
        instance Show (a -> b) where
          show _ = ""
      |] $ do
        (usageGraph <$> parseStringGraph ["Foo.hs"]) `shouldReturn` []

    context "PatBind" $ do
      it "can parse pattern binding" $ do
        withFooHeader [i|
          (Just foo) = let x = x in x
        |] $ do
          parseStringGraph ["Foo.hs"] `shouldReturn`
            Graph [("Foo.foo", ["GHC.Base.Just"])] []

      it "can parse tuple pattern binding" $ do
        withFooHeader [i|
          (a, b) = let x = x in x
        |] $ do
          parseStringGraph ["Foo.hs"] `shouldReturn`
            Graph [("Foo.a", []), ("Foo.b", [])] []

    context "local variables" $ do
      it "recognizes recursive definitions" $ do
        withFooHeader [i|
          foo = foo
          bar = ()
        |] $ do
          parseStringGraph ["Foo.hs"] `shouldReturn`
            Graph [("Foo.bar", ["GHC.Tuple.()"]), ("Foo.foo", ["Foo.foo"])] []
