{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Run where

import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.Version
import           Development.GitRev
import           FastString
import           GHC
import           OccName
import           System.Exit
import           WithCli

import           Ast
import           Files
import           GHC.Show
import           Graph
import qualified Paths_dead_code_detection as Paths
import           Utils

data Options
  = Options {
    sourceDirs :: [FilePath],
    ignoreFiles :: [FilePath],
    root :: [String],
    version :: Bool,
    includeUnderscoreNames :: Bool
  }
  deriving (Show, Eq, Generic)

instance HasArguments Options

run :: IO ()
run = do
  let mods = [AddShortOption "sourceDirs" 'i',
              AddShortOption "ignoreFiles" 'e']
  withCliModified mods $ \ options -> do
    when (version options) $ do
      putStrLn versionOutput
      throwIO ExitSuccess
    when (null $ root options) $
      die "missing option: --root=STRING"
    files <- filter (`notElem` ignoreFiles options) <$>
      findHaskellFiles (sourceDirs options)
    deadNames <- deadNamesFromFiles
      files
      (map mkModuleName (root options))
      (includeUnderscoreNames options)
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

deadNamesFromFiles :: [FilePath] -> [ModuleName] -> Bool -> IO [String]
deadNamesFromFiles files roots includeUnderscoreNames = do
  ast <- parse files
  case ast of
    Left err -> die $ ghcError err
    Right ast -> case findExports ast roots of
      Left err -> die err
      Right rootExports -> do
        let graph = usedTopLevelNames ast
        return $ fmap formatName $
          removeConstructorNames $
          filterUnderScoreNames includeUnderscoreNames $
          deadNames graph rootExports
  where
    ghcError message = stripSpaces $ unlines $
      "Some of the input files produce compile errors." :
      "ghc says:" :
      map ("   " ++) (lines message) ++
      []

filterUnderScoreNames :: Bool -> [Name] -> [Name]
filterUnderScoreNames include = if include then id else
  filter (not . startsWith (== '_'))

startsWith :: (Char -> Bool) -> Name -> Bool
startsWith p name =
  case unpackFS $ occNameFS $ occName name of
    (a : _) -> p a
    [] -> False

removeConstructorNames :: [Name] -> [Name]
removeConstructorNames = filter (not . isConstructorName)

isConstructorName :: Name -> Bool
isConstructorName name =
  startsWith isUpper name ||
  startsWith (== ':') name
