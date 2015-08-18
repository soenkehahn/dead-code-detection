{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module Graph where

import qualified Data.Graph.Wrapper as Wrapper
import           Data.List
import qualified Data.Set as Set
import           Name

newtype Graph a = Graph [(a, [a])]
  deriving (Show, Functor)

instance (Ord a) => Eq (Graph a) where
  Graph a == Graph b = sort a == sort b

sources :: Graph a -> [a]
sources (Graph tuples) = map fst tuples

toWrapperGraph :: Ord a => Graph a -> Wrapper.Graph a ()
toWrapperGraph (Graph g) = Wrapper.fromListLenient $
  map (\ (v, outs) -> (v, (), outs)) g

deadNames :: Graph Name -> [Name] -> [Name]
deadNames (toWrapperGraph -> graph) roots =
  case map (deadNamesSingle graph) roots of
    (x : xs) -> Set.toList $ foldl Set.intersection x xs
    [] -> Wrapper.vertices graph

deadNamesSingle :: Wrapper.Graph Name () -> Name -> Set.Set Name
deadNamesSingle graph root =
  let reachable = Set.fromList $ Wrapper.reachableVertices graph root
      allTopLevelDecls = Set.fromList $ Wrapper.vertices graph
  in allTopLevelDecls Set.\\ reachable
