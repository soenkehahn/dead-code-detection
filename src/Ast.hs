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
import           Data.List
import           Data.Maybe
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
          return $ boundNames declarations
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
  nameGraph group =
    nameGraph (hs_valds group)

instance NameGraph (HsValBinds Name) where
  nameGraph = \ case
    ValBindsOut (map snd -> binds) _signatures -> nameGraph binds
    ValBindsIn _ _ -> error "ValBindsIn shouldn't exist after renaming"

instance NameGraph (HsBindLR Name Name) where
  nameGraph binding =
    map (, nub $ usedNames binding) (boundNames binding)

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
    bind -> nyi "HsBindLR" bind

instance BoundNames (Pat Name) where
  boundNames = \ case
    ParPat p -> boundNames p
    ConPatIn _ p -> boundNames p
    VarPat p -> [p]
    TuplePat pats _ _ -> boundNames pats
    WildPat _ -> []
    pat -> nyi "Pat" pat

instance BoundNames (HsConPatDetails Name) where
  boundNames = \ case
    PrefixCon args -> boundNames args
    InfixCon a b -> boundNames [a, b]
    _ -> error "Not yet implemented: HsConPatDetails"

instance BoundNames (TyClGroup Name) where
  boundNames g = boundNames (universeBi g :: [ConDecl Name])

instance BoundNames (ConDecl Name) where
  boundNames conDecl =
    filter (not . isHidden) $
    concatMap (map unLoc . cd_fld_names) (universeBi conDecl :: [ConDeclField Name])
    where
      isHidden :: Name -> Bool
      isHidden name = "_" `isPrefixOf` occNameString (getOccName name)

-- | extracts names used in instance declarations
getClassMethodUsedNames :: Ast -> [Name]
getClassMethodUsedNames ast =
  concatMap fromInstanceDecl (universeBi ast) ++
  concatMap fromClassDecl (universeBi ast)
  where
    fromInstanceDecl :: InstDecl Name -> [Name]
    fromInstanceDecl = concatMap usedNamesBind . universeBi

    fromClassDecl :: TyClDecl Name -> [Name]
    fromClassDecl = \ case
      ClassDecl{tcdMeths} ->
        concatMap usedNamesBind $ map unLoc $ bagToList tcdMeths
      _ -> []

    usedNamesBind :: HsBindLR Name Name -> [Name]
    usedNamesBind bind = usedNames bind

-- | extracts all used names from ASTs
usedNames :: HsBindLR Name Name -> [Name]
usedNames = catMaybes . map extractHsVar . (universeBi :: HsBindLR Name Name -> [HsExpr Name])
  where
    extractHsVar :: HsExpr Name -> Maybe Name
    extractHsVar (HsVar n) = Just n
    extractHsVar _ = Nothing
