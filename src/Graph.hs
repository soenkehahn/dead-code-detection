{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module Graph where

import qualified Data.Graph.Wrapper as Wrapper
import           Data.List
import           Name

import           GHC.Show

newtype Graph a = Graph [(a, [a])]
  deriving (Show, Functor)

instance (Ord a) => Eq (Graph a) where
  Graph a == Graph b = sort a == sort b

toWrapperGraph :: Ord a => Graph a -> Wrapper.Graph a ()
toWrapperGraph (Graph g) = Wrapper.fromListLenient $
  map (\ (v, outs) -> (v, (), outs)) g

deadNames :: Graph Name -> Name -> [Name]
deadNames (toWrapperGraph -> graph) root =
  let reachable = Wrapper.reachableVertices graph root
      allTopLevelDecls = Wrapper.vertices graph
  in allTopLevelDecls \\ reachable

-- fixme: use Sets?

findName :: Graph Name -> String -> Either String Name
findName graph s = case filter ((== s) . showName) (Wrapper.vertices $ toWrapperGraph graph) of
  [n] -> Right n
