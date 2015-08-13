
module Helper where

import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           Outputable
import           Test.Hspec
import           Test.Mockery.Directory

withFoo :: String -> IO () -> IO ()
withFoo code action = do
  inTempDirectory $ do
    writeFile "Foo.hs" $
      "module Foo where\n" ++
      unindent code
    action
