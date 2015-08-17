{-# LANGUAGE DeriveGeneric #-}

module Run where

import           Control.Monad
import qualified GHC.Generics
import           System.Console.GetOpt.Generics
import           System.Exit

import           Files
import           GHC.Show
import           Graph
import           Parse

data Options
  = Options {
    sourceDirs :: [FilePath]
--    root :: [String]
  }
  deriving (Show, GHC.Generics.Generic)

instance Generic Options
instance HasDatatypeInfo Options

run :: IO ()
run = do
  options <- modifiedGetArguments $
    AddShortOption "sourceDirs" 'i' :
  --  UseForPositionalArguments "root" "ROOT" :
    []
  files <- findHaskellFiles (sourceDirs options)
  deadNames <- deadNamesFromFiles files "Main.main"
  forM_ deadNames putStrLn

deadNamesFromFiles :: [FilePath] -> String -> IO [String]
deadNamesFromFiles files root = do
  parsed <- parse files
  case parsed of
    Right ast -> do
      let graph = nameUsageGraph ast
      case findName graph root of
        Right rootName ->
          return $ fmap formatName $ deadNames graph [rootName]
    Left err -> die err
