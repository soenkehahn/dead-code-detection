
module FilesSpec where

import           Test.Hspec
import           Test.Mockery.Directory

import           Files

spec :: Spec
spec = do
  describe "findHaskellFiles" $ do
    it "returns haskell files in the directory recursively" $ do
      inTempDirectory $ do
        touch "somewhere/Makefile"
        touch "Main.hs"
        touch "somewhere/src/Foo.hs"
        touch "somewhere/Main.hs"
        touch "somewhere/src/Bar.lhs"
        touch "somewhere/assets/baz.png"
        touch "other/Foo.hs"
        findHaskellFiles ["somewhere", "other"] `shouldReturn`
          ["somewhere/src/Foo.hs", "somewhere/Main.hs", "somewhere/src/Bar.lhs", "other/Foo.hs"]
