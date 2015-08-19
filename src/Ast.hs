{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import           Data.List
import qualified GHC
import           GHC hiding (Module, moduleName)
import           GHC.Paths (libdir)
import           Name
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
      return $ boundNames declarations
    [Module _ (Just exports) _] ->
      return $ concatMap (ieNames . unLoc) exports
    [] -> Left ("cannot find module: " ++ moduleNameString name)
    _ -> Left ("found module multiple times: " ++ moduleNameString name)

-- * name usage graph

usedTopLevelNames :: Ast -> Graph Name
usedTopLevelNames ast =
  Graph
    (removeLocalNames (nameGraph ast))
    (instanceUsedNames ast)
  where
    isTopLevelName :: Name -> Bool
    isTopLevelName = maybe False (const True) .  nameModule_maybe

    removeLocalNames :: [(Name, [Name])] -> [(Name, [Name])]
    removeLocalNames =
      filter (isTopLevelName . fst) >>>
      map (second (filter isTopLevelName))

-- | extracts the name usage graph from ASTs
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
  nameGraph = nameGraph . hs_valds

instance NameGraph (HsValBinds Name) where
  nameGraph = \ case
    ValBindsOut (map snd -> binds) _signatures -> nameGraph binds

instance NameGraph (HsBindLR Name Name) where
  nameGraph binding =
    map (, nub $ usedNames bn binding) bn
      where
        bn = boundNames binding

-- | extracts the bound names from ASTs
class BoundNames ast where
  boundNames :: ast -> [Name]

instance (BoundNames a) => BoundNames (Located a) where
  boundNames = boundNames . unLoc

instance (BoundNames a) => BoundNames [a] where
  boundNames = concatMap boundNames

instance BoundNames (HsGroup Name) where
  boundNames group = boundNames (universeBi group :: [HsBindLR Name Name])

instance BoundNames (HsBindLR Name Name) where
  boundNames = \ case
    FunBind id _ _ _ _ _ -> [unLoc id]
    PatBind pat _ _ _ _ -> boundNames pat
    bind -> nyi bind

instance BoundNames (Pat Name) where
  boundNames = \ case
    ParPat p -> boundNames p
    ConPatIn _ p -> boundNames p
    VarPat p -> [p]
    TuplePat pats _ _ -> boundNames pats
    pat -> nyi pat

instance BoundNames (HsConPatDetails Name) where
  boundNames = \ case
    PrefixCon args -> boundNames args
    _ -> error "Not yet implemented: HsConPatDetails"

-- | extracts names used in instance declarations
instanceUsedNames :: Ast -> [Name]
instanceUsedNames = concatMap fromInstanceDecl . universeBi
  where
    fromInstanceDecl :: InstDecl Name -> [Name]
    fromInstanceDecl = concatMap usedNamesBind . universeBi

    usedNamesBind :: HsBindLR Name Name -> [Name]
    usedNamesBind bind = usedNames (boundNames bind) bind

-- | extracts all used names from ASTs
usedNames :: [Name] -> HsBindLR Name Name -> [Name]
usedNames ids = filter (`notElem` ids) . universeBi
