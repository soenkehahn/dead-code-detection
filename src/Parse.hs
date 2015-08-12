
module Parse where

import           GHC
import           GHC.Paths (libdir)

parse :: FilePath -> IO Ast
parse file = runGhc (Just libdir) $ do
  getSessionDynFlags >>= setSessionDynFlags
  guessTarget file Nothing >>= setTargets . pure
  [mod] <- depanal [] False
  renamed <- tm_renamed_source <$> (parseModule mod >>= typecheckModule)
  undefined

type Ast = ()
