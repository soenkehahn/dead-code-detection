{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parse (
  Ast,
  findExports,
  parse,
  usedNames,
  ) where

import           Control.Monad
import           Data.Data
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
  deriving (Data)

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
    void $ setSessionDynFlags $ dynFlags {
      hscTarget = HscNothing,
      ghcLink = NoLink
    }
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
      return $ topLevelNames declarations
    [Module _ (Just exports) _] ->
      return $ concatMap (ieNames . unLoc) exports
    [] -> Left ("cannot find module: " ++ moduleNameString name)
    _ -> Left ("found module multiple times: " ++ moduleNameString name)

-- fixme: Parse -> Ast

-- * name usage graph

topLevelNames :: HsGroup Name -> [Name]
topLevelNames = map (fst . usedNamesFromBinding) . universeBi

usedNames :: Ast -> Graph Name
usedNames = Graph . map usedNamesFromBinding . universeBi

usedNamesFromBinding :: HsBindLR Name Name -> (Name, [Name])
usedNamesFromBinding = \ case
  FunBind id _ matches _ _ _ ->
    (unLoc id, extractNames (unLoc id) matches)

extractNames :: Name -> MatchGroup Name (LHsExpr Name) -> [Name]
extractNames id = filter (/= id) . universeBi
