
module Main where

import           GHC (runGhc)
import           GHC.Paths (libdir)

import qualified Spec

main :: IO ()
main = do
  runGhc (Just libdir) (return ())
  Spec.main
