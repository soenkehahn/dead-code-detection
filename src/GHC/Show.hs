{-# LANGUAGE LambdaCase #-}

module GHC.Show where

import           Data.String.Conversions
import           FastString
import           GHC
import           Name
import           Outputable

formatName :: Name -> String
formatName name =
  srcLocS ++ ": " ++ showSDocUnsafe (ppr name)
  where
    srcLocS = case nameSrcLoc name of
      RealSrcLoc loc ->
        cs (fs_bs (srcLocFile loc)) ++ ":" ++
        show (srcLocLine loc) ++ ":" ++
        show (srcLocCol loc)
      UnhelpfulLoc s -> cs (fs_bs s)
