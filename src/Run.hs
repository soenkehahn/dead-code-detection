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
    root :: [String]
  }
  deriving (Show, Eq, GHC.Generics.Generic)

instance Generic Options
instance HasDatatypeInfo Options

run :: IO ()
run = do
  options <- modifiedGetArguments $
    AddShortOption "sourceDirs" 'i' :
  --  UseForPositionalArguments "root" "ROOT" :
    []
  when (options == Options [] []) $
    die "missing option: --root=STRING"
  files <- findHaskellFiles (sourceDirs options)
  deadNames <- deadNamesFromFiles files (map mkModuleName (root options))
  case deadNames of
    [] -> return ()
    _ -> do
      forM_ deadNames putStrLn
      exitWith $ ExitFailure 1

deadNamesFromFiles :: [FilePath] -> [ModuleName] -> IO [String]
deadNamesFromFiles files roots = do
  ast <- parse files
  case ast of
    Left err -> die err
    Right ast -> case findExports ast roots of
      Left err -> die err
      Right rootExports -> do
        let graph = usedTopLevelNames ast
        return $ fmap formatName $ deadNames graph rootExports
