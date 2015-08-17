
module Helper where

import           Control.Monad
import           Data.String.Interpolate.Util
import           System.FilePath
import           Test.Mockery.Directory

import           GHC.Show
import           Graph
import           Parse

withFoo :: String -> IO () -> IO ()
withFoo code =
  withModules [("Foo", "module Foo where\n" ++ unindent code)]

withModules :: [(String, String)] -> IO () -> IO ()
withModules modules action = do
  inTempDirectory $ do
    forM_ modules $ \ (name, code) -> do
      writeFile (name <.> "hs") (unindent code)
    action

parseStringGraph :: [FilePath] -> IO (Graph String)
parseStringGraph files = do
  result <- parse files
  case result of
    Right r -> return $ fmap showName $ nameUsageGraph r
