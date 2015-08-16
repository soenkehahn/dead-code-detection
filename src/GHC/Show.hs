{-# LANGUAGE LambdaCase #-}

module GHC.Show where

import           ErrUtils
import           GHC
import           HscTypes
import           Name
import           Outputable

formatName :: Name -> String
formatName name =
  file ++ ": " ++ showName name
  where
    file = showSrcLoc $ nameSrcLoc name

showName :: Name -> String
showName name = mod ++ "." ++ id
  where
    mod = maybe "<unknown module>" (showSDocUnsafe . ppr) $
      nameModule_maybe name
    id = showSDocUnsafe $ ppr name

showSrcLoc :: SrcLoc -> String
showSrcLoc = \ case
  RealSrcLoc srcLoc -> show $ srcLocFile srcLoc

showSourceError :: SourceError -> String
showSourceError = unlines . map showSDocUnsafe . pprErrMsgBagWithLoc . srcErrorMessages
