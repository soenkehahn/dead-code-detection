
module Run where

import           Graph
import           Parse

deadNamesFromFiles :: [FilePath] -> String -> IO [String]
deadNamesFromFiles files root = do
  parsed <- parse files
  return $ case parsed of
    Right ast -> deadNames (nameUsageGraph ast) root
