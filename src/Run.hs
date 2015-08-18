{-# LANGUAGE DeriveGeneric #-}

module Run where

import           Control.Monad
import           GHC
import qualified GHC.Generics
import           System.Console.GetOpt.Generics
import           System.Exit

import           Files
import           GHC.Show
import           Graph
import           Ast

data Options
  = Options {
    sourceDirs :: [FilePath],
    root :: String
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
  deadNames <- deadNamesFromFiles files (mkModuleName (root options))
  forM_ deadNames putStrLn

deadNamesFromFiles :: [FilePath] -> ModuleName -> IO [String]
deadNamesFromFiles files root = do
  ast <- parse files
  case ast of
    Left err -> die err
    Right ast -> case findExports ast root of
      Left err -> die err
      Right rootExports -> do
        let graph = usedNames ast
        return $ fmap formatName $ deadNames graph rootExports
