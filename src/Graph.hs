{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module Graph where

import qualified Data.Graph.Wrapper as Wrapper
import           Data.List
import           Name
import           Safe

import           GHC.Show

newtype Graph a = Graph [(a, [a])]
  deriving (Show, Functor)

instance (Ord a) => Eq (Graph a) where
  Graph a == Graph b = sort a == sort b

toWrapperGraph :: Ord a => Graph a -> Wrapper.Graph a ()
toWrapperGraph (Graph g) = Wrapper.fromListLenient $
  map (\ (v, outs) -> (v, (), outs)) g

deadNames :: Graph Name -> String -> [Name]
deadNames (toWrapperGraph -> graph) root =
  let rootName = headNote "deadNames fixme" $ filter ((== root) . showName)
        (Wrapper.vertices graph)
      reachable = Wrapper.reachableVertices graph rootName
      allTopLevelDecls = Wrapper.vertices graph
  in allTopLevelDecls \\ reachable

-- fixme: use Sets?
