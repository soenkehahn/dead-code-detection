{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module Graph where

import           Data.Foldable
import qualified Data.Graph.Wrapper as Wrapper
import           Data.List
import qualified Data.Set as Set
import           Name

data Graph a = Graph {
  usageGraph :: [(a, [a])],
  instanceMethodNames :: [a]
} deriving (Show, Functor)

instance (Ord a) => Eq (Graph a) where
  Graph a aS == Graph b bS =
    sort a == sort b &&
    aS == bS

toWrapperGraph :: Ord a => Graph a -> Wrapper.Graph a ()
toWrapperGraph (Graph g _) = Wrapper.fromListLenient $
  map (\ (v, outs) -> (v, (), outs)) g

deadNames :: Graph Name -> [Name] -> [Name]
deadNames g@(toWrapperGraph -> graph) roots =
  case map (deadNamesSingle graph) (roots ++ toList (instanceMethodNames g)) of
    (x : xs) -> Set.toList $ foldl Set.intersection x xs
    [] -> Wrapper.vertices graph

deadNamesSingle :: Wrapper.Graph Name () -> Name -> Set.Set Name
deadNamesSingle graph root =
  let reachable = Set.fromList $ Wrapper.reachableVertices graph root
      allTopLevelDecls = Set.fromList $ Wrapper.vertices graph
  in allTopLevelDecls Set.\\ reachable
