{-# LANGUAGE LambdaCase #-}

module GHC.Show where

import           Data.String.Conversions
import           ErrUtils
import           FastString
import           GHC
import           HscTypes
import           Name
import           Outputable

formatName :: Name -> String
formatName name =
  srcLocS ++ ": " ++ showName name
  where
    srcLocS = case nameSrcLoc name of
      RealSrcLoc loc ->
        cs (fs_bs (srcLocFile loc)) ++ ":" ++
        show (srcLocLine loc) ++ ":" ++
        show (srcLocCol loc)

showName :: Name -> String
showName name = mod ++ "." ++ id
  where
    mod = maybe "<unknown module>" (showSDocUnsafe . ppr) $
      nameModule_maybe name
    id = showSDocUnsafe $ ppr name

showSourceError :: SourceError -> String
showSourceError = unlines . map showSDocUnsafe . pprErrMsgBagWithLoc . srcErrorMessages

-- * development utils

ppe :: Outputable doc => doc -> a
ppe = error . showSDocUnsafe . ppr
