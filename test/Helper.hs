
module Helper where

import           Data.String.Interpolate.Util
import           Test.Mockery.Directory

withFoo :: String -> IO () -> IO ()
withFoo code action = do
  inTempDirectory $ do
    writeFile "Foo.hs" $
      "module Foo where\n" ++
      unindent code
    action
