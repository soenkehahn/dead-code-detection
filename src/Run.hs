{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Run where

import           Control.Exception
import           Control.Monad
import           Data.Version
import           Development.GitRev
import           GHC
import qualified GHC.Generics
import           System.Console.GetOpt.Generics
import           System.Exit

import           Ast
import           Files
import           GHC.Show
import           Graph
import qualified Paths_dead_code_detection as Paths

data Options
  = Options {
    sourceDirs :: [FilePath],
    root :: [String],
    version :: Bool
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
  when (options == Options [] [] False) $
    die "missing option: --root=STRING"
  when (version options) $ do
    putStrLn versionOutput
    throwIO ExitSuccess
  files <- findHaskellFiles (sourceDirs options)
  deadNames <- deadNamesFromFiles files (map mkModuleName (root options))
  case deadNames of
    [] -> return ()
    _ -> do
      forM_ deadNames putStrLn
      exitWith $ ExitFailure 1

versionOutput :: String
versionOutput =
  "version: " ++ full
  where
    isInGit = $(gitHash) /= "UNKNOWN"
    full = if isInGit
      then "version: " ++ showVersion Paths.version ++ "\n" ++
           "rev: " ++ $(gitHash) ++ (if $(gitDirty) then " (dirty)" else "") ++ "\n" ++
           "branch: " ++ $(gitBranch)
      else "version: " ++ showVersion Paths.version

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
