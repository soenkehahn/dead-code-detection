
module Parse where

import           GHC
import           GHC.Paths (libdir)

parse :: FilePath -> IO (Either String Ast)
parse file =
  handleSourceError foo $
  runGhc (Just libdir) $ do
    getSessionDynFlags >>= setSessionDynFlags
    guessTarget file Nothing >>= setTargets . pure
    [mod] <- depanal [] False
    renamed <- tm_renamed_source <$> (parseModule mod >>= typecheckModule)
    error "not in scope: bar"
    maybe (error "fixme") (\ (a, _, _, _) -> return a) renamed

type Ast = HsGroup Name

handleSourceError :: IO a -> IO a
handleSourceError = id
