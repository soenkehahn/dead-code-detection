{-# LANGUAGE ViewPatterns #-}

module Graph where

import           Data.Graph.Wrapper
import           Data.List

deadNames :: [(String, [String])] -> String -> [String]
deadNames (fromListLenient . map (\ (a, b) -> (a, a, b)) -> graph) root =
  let reachable = reachableVertices graph root 
      allTopLevelDecls = vertices graph
  in allTopLevelDecls \\ reachable

-- fixme: use Sets?
