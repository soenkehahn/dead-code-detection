{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import           Data.Char
import           Data.Set

nubOrd :: forall a . Ord a => [a] -> [a]
nubOrd = inner empty
  where
    inner :: Set a -> [a] -> [a]
    inner acc (a : r)
      | a `member` acc = inner acc r
      | otherwise = a : inner (insert a acc) r
    inner _ [] = []

errorNyi :: String -> a
errorNyi message = error $ stripSpaces $ unlines $
  "Encountered a language construct that is" :
  "not yet implemented. Please consider opening a bug report about" :
  "this here: https://github.com/soenkehahn/dead-code-detection/issues" :
  "" :
  "Here's some debugging output that will probably help to solve this problem:" :
  message :
  []

stripSpaces :: String -> String
stripSpaces =
  reverse . dropWhile isSpace .
  reverse . dropWhile isSpace
