
module Helper where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Compat
import           Data.String.Interpolate.Util
import           System.FilePath
import           Test.Mockery.Directory

withFoo :: String -> IO () -> IO ()
withFoo code =
  withModules [("Foo", "module Foo where\n" ++ unindent code)]

withModules :: [(String, String)] -> IO () -> IO ()
withModules modules action = do
  inTempDirectory $ do
    forM_ modules $ \ (name, code) -> do
      writeFile (name <.> "hs") (unindent code)
    action
