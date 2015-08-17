{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parse (
  Ast,
  showName,
  parse,
  nameUsageGraph,
  ) where

import           Bag
import           BasicTypes
import           Control.Monad
import           Data.Generics.Uniplate.Data
import           GHC
import           GHC.Paths (libdir)
import           System.IO
import           System.IO.Silently

import           GHC.Show
import           Graph

parse :: [FilePath] -> IO (Either String Ast)
parse files =
  handleSourceError (return . Left . showSourceError) $
  hSilence [stdout, stderr] $
  runGhc (Just libdir) $ do
    dynFlags <- getSessionDynFlags
    void $ setSessionDynFlags (dynFlags { ghcLink = NoLink })
    targets <- forM files $ \ file -> guessTarget file Nothing
    setTargets targets
    modSummaries <- depanal [] False
    _ <- load LoadAllTargets
    let isModuleFromFile m = case ml_hs_file $ ms_location m of
          Nothing -> error ("parse: module without file")
          Just file -> file `elem` files
        mods = filter isModuleFromFile modSummaries
    renamed <- forM mods $ \ mod ->
      (tm_renamed_source <$> (parseModule mod >>= typecheckModule))
    Right <$>
      mapM (maybe (error "fixme") (\ (a, _, _, _) -> return a)) renamed

type Ast = [HsGroup Name]

-- * name usage graph

-- fixme: rename things

nameUsageGraph :: NUG ast => ast -> Graph Name
nameUsageGraph = Graph . nug

class NUG ast where
  nug :: ast -> [(Name, [Name])]

instance NUG a => NUG (Located a) where
  nug = nug . unLoc

instance NUG a => NUG [a] where
  nug = concatMap nug

instance (NUG a, NUG b) => NUG (a, b) where
  nug (a, b) = nug a ++ nug b

instance NUG a => NUG (Bag a) where
  nug = nug . bagToList

instance NUG (HsGroup Name) where
  nug = nug . hs_valds
  -- fixme: other fields

instance NUG (HsValBinds Name) where
  nug = \ case
    ValBindsOut a _b ->
      nug a
      -- fixme: ++ nug b

instance NUG RecFlag where
  nug _ = []

instance NUG (HsBindLR Name Name) where
  nug = \ case
    FunBind id _ matches _ _ _ ->
      [(unLoc id, usedNames matches)]

-- * extracting used names

class UN ast where
  usedNames :: ast -> [Name]

instance UN a => UN (Located a) where
  usedNames = usedNames . unLoc

instance UN a => UN [a] where
  usedNames = concatMap usedNames

instance UN (MatchGroup Name (LHsExpr Name)) where
  usedNames = usedNames . mg_alts

instance UN (Match Name (LHsExpr Name)) where
  usedNames = usedNames . m_grhss

instance UN (GRHSs Name (LHsExpr Name)) where
  usedNames = universeBi
