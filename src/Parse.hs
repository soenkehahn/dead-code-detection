{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parse (
  Ast,
  findExports,
  parse,
  nameUsageGraph,
  ) where

import           Bag
import           BasicTypes
import           Control.Monad
import           Data.Generics.Uniplate.Data
import qualified GHC
import           GHC hiding (Module, moduleName)
import           GHC.Paths (libdir)
import           Outputable
import           System.IO
import           System.IO.Silently

import           GHC.Show
import           Graph

type Ast = [Module]

data Module
  = Module {
    moduleName :: ModuleName,
    moduleExports :: Maybe [LIE Name],
    moduleDeclarations :: HsGroup Name
  }

instance Outputable Module where
  ppr m =
    text "Module" <+>
    ppr (moduleName m) <+>
    ppr (moduleExports m) <+>
    brackets (ppr (moduleDeclarations m))

toModule :: TypecheckedModule -> Module
toModule m = case tm_renamed_source m of
  Just (hsGroup, _, exports, _) ->
    Module
      (GHC.moduleName $ ms_mod $ pm_mod_summary $ tm_parsed_module m)
      exports
      hsGroup
  Nothing -> error "tm_renamed_source should point to a renamed source after renaming"

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
    typecheckedModules <- forM mods $ \ mod ->
      (parseModule mod >>= typecheckModule)
    return $ Right $ map toModule typecheckedModules

findExports :: Ast -> ModuleName -> Either String [Name]
findExports ast name =
  case filter (\ m -> moduleName m == name) ast of
    [Module _ Nothing declarations] ->
      return $ sources $ nameUsageGraph declarations
    [Module _ (Just exports) _] ->
      return $ concatMap (ieNames . unLoc) exports
    [] -> Left ("cannot find module: " ++ moduleNameString name)
    _ -> Left ("found module multiple times: " ++ moduleNameString name)

-- fixme: Parse -> Ast

-- * name usage graph

-- fixme: rename things

nameUsageGraph :: NUG ast => ast -> Graph Name
nameUsageGraph = Graph . nug

class NUG ast where
  nug :: ast -> [(Name, [Name])]

instance NUG a => NUG (Maybe a) where
  nug = maybe [] nug

instance NUG a => NUG (Located a) where
  nug = nug . unLoc

instance NUG a => NUG [a] where
  nug = concatMap nug

instance (NUG a, NUG b) => NUG (a, b) where
  nug (a, b) = nug a ++ nug b

instance NUG a => NUG (Bag a) where
  nug = nug . bagToList

instance NUG Module where
  nug = nug . moduleDeclarations

instance NUG (HsGroup Name) where
  nug = nug . hs_valds
  -- fixme: other fields

instance NUG (HsValBinds Name) where
  nug = \ case
    ValBindsOut binds _signatures ->
      nug binds
    ValBindsIn _ _ -> error "ValBindsIn shouldn't exist after renaming"

instance NUG RecFlag where
  nug _ = []

instance NUG (IE Name) where
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
  usedNames ast = usedNames (m_pats ast) ++ usedNames (m_grhss ast)

instance UN (GRHSs Name (LHsExpr Name)) where
  usedNames = universeBi

instance UN (Pat Name) where
  usedNames = universeBi
