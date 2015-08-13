{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parse where

import           Bag
import           BasicTypes
import           Control.Arrow
import           Data.Data
import           Data.Generics.Uniplate.Data
import           ErrUtils
import           GHC
import           GHC.Paths (libdir)
import           HsBinds
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

showName :: Name -> String
showName = showSDocUnsafe . ppr

nameUsageGraph :: NUG ast => ast -> [(String, [String])]
nameUsageGraph = map (first showName . second (map showName)) . nug

class NUG ast where
  nug :: ast -> [(Name, [Name])]

instance NUG Ast where
  nug = nug . hs_valds
  -- fixme: other fields

instance NUG (HsValBinds Name) where
  nug = \ case
    ValBindsOut a b ->
      nug a
      -- fixme: ++ nug b

instance NUG RecFlag where
  nug _ = []

instance NUG a => NUG [a] where
  nug = concatMap nug

instance (NUG a, NUG b) => NUG (a, b) where
  nug (a, b) = nug a ++ nug b

instance NUG a => NUG (Bag a) where
  nug = nug . bagToList

instance NUG a => NUG (Located a) where
  nug = nug . unLoc

instance NUG (HsBindLR Name Name) where
  nug = \ case
    FunBind id _ matches _ _ _ ->
      [(unLoc id, usedNames matches)]

class UN ast where
  usedNames :: ast -> [Name]

instance UN (MatchGroup Name (LHsExpr Name)) where
  usedNames = usedNames . mg_alts

instance UN a => UN [a] where
  usedNames = concatMap usedNames

instance UN a => UN (Located a) where
  usedNames = usedNames . unLoc

instance UN (Match Name (LHsExpr Name)) where
  usedNames = usedNames . m_grhss

instance UN (GRHSs Name (LHsExpr Name)) where
  usedNames = universeBi

-- * development utils

ppe :: Outputable doc => doc -> a
ppe = error . showSDocUnsafe . ppr
