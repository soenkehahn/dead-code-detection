{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Ast (
  Ast,
  findExports,
  parse,
  usedTopLevelNames,
  ) where

import           Bag
import           Control.Arrow ((>>>), second)
import           Control.Monad
import           Data.Data
import           Data.Generics.Uniplate.Data
import qualified GHC
import           GHC hiding (Module, moduleName)
import           GHC.Paths (libdir)
import           Name
import           Outputable
import           System.IO
import           System.IO.Silently

import           Ast.UsedNames
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
  errorHandler $
  runGhc (Just libdir) $ do
    dynFlags <- getSessionDynFlags
    void $ setSessionDynFlags $ dynFlags {
      hscTarget = HscNothing,
      ghcLink = NoLink
    }
    targets <- forM files $ \ file -> guessTarget file Nothing
    setTargets targets
    modSummaries <- depanal [] False
    r <- load LoadAllTargets
    case r of
      Failed -> return Nothing
      Succeeded -> do
        let isModuleFromFile m = case ml_hs_file $ ms_location m of
              Nothing -> error ("parse: module without file")
              Just file -> file `elem` files
            mods = filter isModuleFromFile modSummaries
        typecheckedModules <- forM mods $ \ mod ->
          (parseModule mod >>= typecheckModule)
        return $ Just $ map toModule typecheckedModules

errorHandler :: IO (Maybe a) -> IO (Either String a)
errorHandler action = do
  (errs, a) <- hCapture [stderr] action
  return $ maybe (Left errs) Right a

findExports :: Ast -> [ModuleName] -> Either String [Name]
findExports ast names = concat <$> mapM inner names
  where
    inner name =
      case filter (\ m -> moduleName m == name) ast of
        [Module _ Nothing declarations] ->
          return $ map fst $ nameGraph declarations
        [Module _ (Just exports) _] ->
          concat <$> mapM (extractExportedNames ast . unLoc) exports
        [] -> Left ("cannot find module: " ++ moduleNameString name)
        _ -> Left ("found module multiple times: " ++ moduleNameString name)

extractExportedNames :: Ast -> IE Name -> Either String [Name]
extractExportedNames ast = \ case
  IEModuleContents (unLoc -> moduleName) ->
    findExports ast [moduleName]
  x -> return $ ieNames x

-- * name usage graph

usedTopLevelNames :: Ast -> Graph Name
usedTopLevelNames ast =
  Graph
    (removeLocalNames (nameGraph ast))
    (getClassMethodUsedNames ast)
  where
    isTopLevelName :: Name -> Bool
    isTopLevelName = maybe False (const True) .  nameModule_maybe

    removeLocalNames :: [(Name, [Name])] -> [(Name, [Name])]
    removeLocalNames =
      filter (isTopLevelName . fst) >>>
      map (second (filter isTopLevelName))

-- | extracts the name usage graph from ASTs (only value level)
class NameGraph ast where
  nameGraph :: ast -> [(Name, [Name])]

instance NameGraph a => NameGraph [a] where
  nameGraph = concatMap nameGraph

instance NameGraph a => NameGraph (Bag a) where
  nameGraph = concatMap nameGraph . bagToList

instance NameGraph a => NameGraph (Located a) where
  nameGraph = nameGraph . unLoc

instance NameGraph Module where
  nameGraph = nameGraph . moduleDeclarations

instance NameGraph (HsGroup Name) where
  nameGraph = \ case
    HsGroup valBinds [] tyclds _instances [] [] [] foreign_decls [] [] [] [] [] ->
      nameGraph valBinds ++
      nameGraph tyclds ++
      nameGraph foreign_decls
    x -> o x

instance NameGraph (ForeignDecl Name) where
  nameGraph = \ case
    ForeignImport name _ _ _ -> [(unLoc name, [])]
    x -> o x

instance NameGraph (HsValBinds Name) where
  nameGraph = \ case
    ValBindsOut (map snd -> binds) _signatures -> nameGraph binds
    ValBindsIn _ _ -> error "ValBindsIn shouldn't exist after renaming"

instance NameGraph (HsBindLR Name Name) where
  nameGraph bind = addUsedNames (usedNames bind) $ case bind of
    FunBind id _ _ _ _ _ -> withoutUsedNames [unLoc id]
    PatBind pat _ _ _ _ -> nameGraph pat
    x -> o x

instance NameGraph (Pat Name) where
  nameGraph = \ case
    ParPat p -> nameGraph p
    ConPatIn _ p -> nameGraph p
    VarPat p -> withoutUsedNames [p]
    TuplePat pats _ _ -> nameGraph pats
    WildPat _ -> []
    pat -> nyi "Pat" pat

instance NameGraph (TyClGroup Name) where
  nameGraph = \ case
    TyClGroup decls [] -> nameGraph decls
    x -> o x

instance NameGraph (TyClDecl Name) where
  nameGraph = \ case
    DataDecl _typeCon _ def _ -> nameGraph def
    ClassDecl{} -> []
    SynDecl{} -> []
    x -> o x

instance NameGraph (HsDataDefn Name) where
  nameGraph = \ case
    (HsDataDefn _ _ _ _ constructors _) -> nameGraph constructors

instance NameGraph (ConDecl Name) where
  nameGraph = \ case
    ConDecl names _ _ _ details _ _ _ ->
      withoutUsedNames (map unLoc names) ++
      nameGraph details

instance NameGraph (HsConDetails (LBangType Name) (Located [LConDeclField Name])) where
  nameGraph = \ case
    RecCon rec -> nameGraph rec
    PrefixCon _ -> []
    x -> e x

instance NameGraph (ConDeclField Name) where
  nameGraph = \ case
    ConDeclField names _typ _docs ->
      withoutUsedNames $ map unLoc names

instance NameGraph (HsConPatDetails Name) where
  nameGraph = \ case
    PrefixCon args -> nameGraph args
    InfixCon a b -> nameGraph a ++ nameGraph b
    _ -> error "Not yet implemented: HsConPatDetails"

-- | extracts names used in instance declarations
getClassMethodUsedNames :: Ast -> [Name]
getClassMethodUsedNames ast =
  concatMap fromInstanceDecl (universeBi ast) ++
  concatMap fromClassDecl (universeBi ast)
  where
    fromInstanceDecl :: InstDecl Name -> [Name]
    fromInstanceDecl decl =
      usedNames (universeBi decl :: [HsBindLR Name Name])

    fromClassDecl :: TyClDecl Name -> [Name]
    fromClassDecl = \ case
      ClassDecl{tcdMeths} ->
        usedNames $ map unLoc $ bagToList tcdMeths
      _ -> []
