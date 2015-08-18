{-# language QuasiQuotes #-}

module GraphSpec where

import           Data.String.Interpolate
import           GHC
import           Test.Hspec

import           GHC.Show
import           Graph
import           Helper
import           Ast

spec :: Spec
spec = do
  describe "deadNames" $ do
    it "detects unused top-level names" $ do
      withFoo [i|
        module Foo (foo) where
        foo = ()
        bar = ()
      |] $ do
        Right ast <- parse ["Foo.hs"]
        let graph = usedNames ast
            Right roots = findExports ast (mkModuleName "Foo")
        fmap showName (deadNames graph roots)
          `shouldBe` ["Foo.bar"]

    it "allows to specify multiple roots" $ do
      withFoo [i|
        module Foo (r1, r2) where
        r1 = foo
        r2 = bar
        foo = ()
        bar = ()
        baz = ()
      |] $ do
        Right ast <- parse ["Foo.hs"]
        let graph = usedNames ast
            Right roots = findExports ast (mkModuleName "Foo")
        fmap showName (deadNames graph roots)
          `shouldBe` ["Foo.baz"]
