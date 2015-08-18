
module Helper where

import           Control.Monad
import           Data.String.Interpolate.Util
import           GHC
import           Name
import           Outputable
import           System.Exit
import           System.FilePath
import           Test.Mockery.Directory

import           Graph
import           Ast

withFoo :: String -> IO () -> IO ()
withFoo code =
  withModules [("Foo", unindent code)]

withFooHeader :: String -> IO () -> IO ()
withFooHeader code =
  withFoo ("module Foo where\n" ++ code)

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
    Left e -> die e
    Right r -> return $ fmap showName $ usedTopLevelNames r

showName :: Name -> String
showName name = mod ++ "." ++ id
  where
    mod = maybe "<unknown module>" (showSDocUnsafe . ppr) $
      nameModule_maybe name
    id = showSDocUnsafe $ ppr name
