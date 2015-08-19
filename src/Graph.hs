{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module Graph where

import qualified Data.Graph.Wrapper as Wrapper
import           Data.Graph.Wrapper hiding (Graph, toList)
import           Data.List
import qualified Data.Set as Set
import           Name

data Graph a = Graph {
  _usageGraph :: [(a, [a])],
  classMethodsUsedNames :: [a]
} deriving (Show, Functor)

instance (Ord a) => Eq (Graph a) where
  Graph a aUseds == Graph b bUseds =
    a === b &&
    aUseds === bUseds
    where
      x === y = sort (nub x) == sort (nub y)

toWrapperGraph :: Ord a => Graph a -> Wrapper.Graph a ()
toWrapperGraph (Graph g _) = fromListLenient $
  map (\ (v, outs) -> (v, (), outs)) g

deadNames :: Graph Name -> [Name] -> [Name]
deadNames g@(toWrapperGraph -> graph) roots =
  sortTopologically graph $
  case map (deadNamesSingle graph) (roots ++ classMethodsUsedNames g) of
    (x : xs) -> foldl Set.intersection x xs
    [] -> Set.fromList $ vertices graph

deadNamesSingle :: Wrapper.Graph Name () -> Name -> Set.Set Name
deadNamesSingle graph root =
  let reachable = Set.fromList $ reachableVertices graph root
      allTopLevelDecls = Set.fromList $ vertices graph
  in allTopLevelDecls Set.\\ reachable

sortTopologically :: Ord a => Wrapper.Graph a () -> Set.Set a -> [a]
sortTopologically graph set =
  filter (`Set.member` set) (topologicalSort graph)
