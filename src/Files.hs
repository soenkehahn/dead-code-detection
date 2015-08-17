
module Files where

import           System.FilePath.Glob

findHaskellFiles :: [FilePath] -> IO [FilePath]
findHaskellFiles sourceDirs = concat <$> mapM inner sourceDirs
  where
    inner sourceDir =
      concat <$> fst <$>
      globDir patterns sourceDir
    patterns = map compile $
      "**/*.hs" :
      "**/*.lhs" :
      []
