
module Run where

import           Control.Monad
import           System.Environment
import           System.Exit

import           Graph
import           Parse

run :: IO ()
run = do
  files <- getArgs
  deadNames <- deadNamesFromFiles files "Main.main"
  forM_ deadNames putStrLn

deadNamesFromFiles :: [FilePath] -> String -> IO [String]
deadNamesFromFiles files root = do
  parsed <- parse files
  case parsed of
    Right ast -> return $ deadNames (fmap showName $ nameUsageGraph ast) root
    Left err -> die err
