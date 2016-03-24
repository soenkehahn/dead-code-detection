{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Ast.UsedNames where

import           Bag
import           Data.Data
import           GHC
import           Outputable
import           Utils

class UsedNames ast where
  -- | extracts all used names from ASTs
  usedNames :: ast -> [Name]

instance UsedNames a => UsedNames [a] where
  usedNames = concatMap usedNames

instance UsedNames a => UsedNames (Bag a) where
  usedNames = concatMap usedNames . bagToList

instance UsedNames a => UsedNames (Located a) where
  usedNames = usedNames . unLoc

instance UsedNames (HsBindLR Name Name) where
  usedNames = \ case
    FunBind _ _ matches _ _ _ -> usedNames matches
    PatBind lhs rhs _ _ _ ->
      usedNames lhs ++ usedNames rhs
    x -> errorNyiOutputable x

instance UsedNames (MatchGroup Name (LHsExpr Name)) where
  usedNames = usedNames . mg_alts

instance UsedNames (Match Name (LHsExpr Name)) where
  usedNames = \ case
    Match _ pats _ rhs ->
      usedNames pats ++ usedNames rhs

instance UsedNames (Pat Name) where
  usedNames = \ case
    ParPat e -> usedNames e
    ViewPat function expr _ ->
      usedNames function ++ usedNames expr
    ConPatIn a b -> unLoc a : usedNames b
    VarPat{} -> []
    TuplePat exprs _ _ -> usedNames exprs
    WildPat _ -> []
    AsPat _as pat -> usedNames pat
    ListPat pats _ _ -> usedNames pats
    SigPatIn pat _sig -> usedNames pat
    BangPat pat -> usedNames pat
    NPat{} -> []
    LitPat{} -> []
    x -> errorNyiOutputable x

instance UsedNames (HsConDetails (LPat Name) (HsRecFields Name (LPat Name))) where
  usedNames = \ case
    PrefixCon args -> usedNames args
    InfixCon a b ->
      usedNames a ++ usedNames b
    RecCon x -> usedNames x

instance UsedNames (GRHSs Name (LHsExpr Name)) where
  usedNames (GRHSs rhss whereClause) =
    usedNames rhss ++ usedNames whereClause

instance UsedNames (GRHS Name (LHsExpr Name)) where
  usedNames (GRHS guards body) =
    usedNames guards ++ usedNames body

instance UsedNames (StmtLR Name Name (LHsExpr Name)) where
  usedNames = \ case
    BindStmt pat expr _ _ ->
      usedNames pat ++ usedNames expr
    LastStmt expr _ -> usedNames expr
    BodyStmt expr _ _ _ -> usedNames expr
    LetStmt x -> usedNames x
    x -> errorNyiOutputable x

instance UsedNames (HsExpr Name) where
  usedNames = \ case
    HsVar n -> [n]
    HsLet binds expr ->
      usedNames binds ++ usedNames expr
    HsLit _ -> []
    HsApp f x -> usedNames f ++ usedNames x
    OpApp a op _ b ->
      usedNames a ++ usedNames op ++ usedNames b
    HsPar x -> usedNames x
    ExplicitList _ Nothing list -> usedNames list
    ExplicitTuple exprs _ -> usedNames exprs
    HsOverLit{} -> []
    HsLam x -> usedNames x
    HsDo context stmts _ ->
      usedNames context ++ usedNames stmts
    SectionL a b ->
      usedNames a ++ usedNames b
    SectionR a b ->
      usedNames a ++ usedNames b
    HsCase on matchGroup ->
      usedNames on ++ usedNames matchGroup
    RecordUpd expr recordBinds [] _ _ ->
      usedNames expr ++ usedNames recordBinds
    HsLamCase _ matchGroup -> usedNames matchGroup
    HsIf _ c t e ->
      usedNames c ++ usedNames t ++ usedNames e
    ExprWithTySig expr _ _ -> usedNames expr
    NegApp expr _ -> usedNames expr
    ArithSeq _ _ info -> usedNames info
    RecordCon constructor _ binds ->
      unLoc constructor : usedNames binds
    x -> errorNyiOutputable x

instance UsedNames (ArithSeqInfo Name) where
  usedNames = \ case
    From f -> usedNames f
    FromThen f t -> usedNames f ++ usedNames t
    FromTo f t -> usedNames f ++ usedNames t
    FromThenTo f t to
      -> usedNames f ++ usedNames t ++ usedNames to

instance UsedNames (HsStmtContext Name) where
  usedNames = \ case
    ListComp -> []
    MonadComp -> []
    PArrComp -> []
    DoExpr -> []
    MDoExpr -> []
    ArrowExpr -> []
    GhciStmtCtxt -> []
    x -> errorNyiData x

instance UsedNames (HsLocalBinds Name) where
  usedNames = \ case
    EmptyLocalBinds -> []
    HsValBinds binds -> usedNames binds
    x -> errorNyiOutputable x

instance UsedNames (HsValBindsLR Name Name) where
  usedNames = \ case
    ValBindsOut (map snd -> binds) _sig ->
      usedNames binds
    x -> errorNyiOutputable x

instance UsedNames (HsTupArg Name) where
  usedNames = \ case
    Present x -> usedNames x
    Missing _ -> []

instance UsedNames arg => UsedNames (HsRecFields Name arg) where
  usedNames = \ case
    HsRecFields fields _ -> usedNames fields

instance UsedNames arg => UsedNames (HsRecField Name arg) where
  usedNames (HsRecField assigned expr _) =
    unLoc assigned : usedNames expr

errorNyiData :: (Data a) => a -> b
errorNyiData x = errorNyi $ ("errorNyiData: " ++ ) $ unlines $
  dataTypeName (dataTypeOf x) :
  show (toConstr x) :
  []

errorNyiOutputable :: (Outputable a, Data a) => a -> b
errorNyiOutputable x = errorNyi $ ("errorNyiOutputable: " ++) $ unlines $
  dataTypeName (dataTypeOf x) :
  show (toConstr x) :
  showSDocUnsafe (ppr x) :
  []
