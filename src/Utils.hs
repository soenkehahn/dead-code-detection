{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import           Data.Set

nubOrd :: forall a . Ord a => [a] -> [a]
nubOrd = inner empty
  where
    inner :: Set a -> [a] -> [a]
    inner acc (a : r)
      | a `member` acc = inner acc r
      | otherwise = a : inner (insert a acc) r
    inner _ [] = []
