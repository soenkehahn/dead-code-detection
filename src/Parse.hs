
module Parse where

import           ErrUtils
import           GHC
import           GHC.Paths (libdir)
import           HscTypes
import           Outputable

parse :: FilePath -> IO (Either String Ast)
parse file =
  handleSourceError (return . Left . showSourceError) $
  runGhc (Just libdir) $ do
    getSessionDynFlags >>= setSessionDynFlags
    guessTarget file Nothing >>= setTargets . pure
    [mod] <- depanal [] False
    renamed <- tm_renamed_source <$> (parseModule mod >>= typecheckModule)
    Right <$>
      maybe (error "fixme") (\ (a, _, _, _) -> return a) renamed

showSourceError :: SourceError -> String
showSourceError = unlines . map showSDocUnsafe . pprErrMsgBagWithLoc . srcErrorMessages

type Ast = HsGroup Name
