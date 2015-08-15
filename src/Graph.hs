{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module Graph where

import qualified Data.Graph.Wrapper as Wrapper
import           Data.List

newtype Graph a = Graph [(a, [a])]
  deriving (Show, Functor)

instance (Ord a) => Eq (Graph a) where
  Graph a == Graph b = sort a == sort b

deadNames :: Graph String -> String -> [String]
deadNames (Graph (Wrapper.fromListLenient . map (\ (a, b) -> (a, a, b)) -> graph)) root =
  let reachable = Wrapper.reachableVertices graph root
      allTopLevelDecls = Wrapper.vertices graph
  in allTopLevelDecls \\ reachable

-- fixme: use Sets?
