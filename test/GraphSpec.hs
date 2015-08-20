{-# language QuasiQuotes #-}

module GraphSpec where

import           Data.String.Interpolate
import           GHC
import           Test.Hspec

import           Graph
import           Helper
import           Ast

getDeadNames :: IO [String]
getDeadNames = do
  ast <- eitherToError $ parse ["Foo.hs"]
  let graph = usedTopLevelNames ast
  roots <- eitherToError $ return $
    findExports ast [mkModuleName "Foo"]
  return $ fmap showName $ deadNames graph roots

spec :: Spec
spec = do
  describe "deadNames" $ do
    it "detects unused top-level names" $ do
      withFoo [i|
        module Foo (foo) where
        foo = ()
        bar = ()
      |] $ do
        getDeadNames `shouldReturn` ["Foo.bar"]

    it "allows to specify multiple roots" $ do
      withFoo [i|
        module Foo (r1, r2) where
        r1 = foo
        r2 = bar
        foo = ()
        bar = ()
        baz = ()
      |] $ do
        getDeadNames `shouldReturn` ["Foo.baz"]

    it "detects usage of names in instance methods" $ do
      withFoo [i|
        module Foo () where
        data A = A
        instance Show A where
          show A = foo
        foo = "foo"
      |] $ do
        getDeadNames `shouldReturn` []

    it "returns dead names in topological order" $ do
      withFoo [i|
        module Foo () where
        b = c
        a = b
        c = ()
      |] $ do
        getDeadNames `shouldReturn` (words "Foo.a Foo.b Foo.c")

    it "finds used names in default implementations of methods in class declarations" $ do
      withFoo [i|
        module Foo () where
        class A a where
          a :: a -> String
          a _ = foo
        foo = "foo"
        |] $ do
          getDeadNames `shouldReturn` []
