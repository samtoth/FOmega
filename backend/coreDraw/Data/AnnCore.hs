{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Data.AnnCore where

import CoreSyn hiding (AnnBind, AnnExpr, AnnType, AnnAlt)
import Literal
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding as BS
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import FastString
import GHC
import Module
import OccName
import TyCoRep
import qualified CoreUtils
import Var
import Util (partitionWith)

data AnnModule = MkAM
  { am_mod :: CoreModule,
    am_name :: T.Text,
    am_mainBinds :: [AnnBindGroup],
    am_specialBinds :: [AnnBindGroup]
  }

data AnnBindGroup
  = ABRec [AnnBind]
  | ABNonRec AnnBind

{-# ANN abg_depth ("HLint: ignore" :: String) #-}
abg_depth :: AnnBindGroup -> Int
abg_depth = \case
            ABRec b -> foldr (\a b -> max (ab_depth a)  b) 0 b
            ABNonRec b ->  ab_depth b

data BindLevel = TermB | TypeB

data AnnBind = MkAB
  { ab_bndr :: AnnBndr,
    ab_expr :: AnnExpr AnnTerm
  }

data AnnBndr =  TermBndr T.Text (AnnExpr AnnType) Bool
                | TypeBndr T.Text (AnnExpr AnnType) Bool

ab_name :: AnnBndr -> T.Text
ab_name = \case
  TermBndr n _ _ -> n
  TypeBndr n _ _ -> n

{-# ANN ab_depth ("HLint: ignore" :: String) #-}
ab_depth :: AnnBind -> Int
ab_depth  = ae_depth.ab_expr

newtype AnnAlt = MkAA (AltCon, [AnnBndr], AnnExpr AnnTerm)

type AnnCo = Coercion


data AnnTerm = IApp (AnnExpr AnnTerm) (AnnExpr AnnTerm)
               | ILam  AnnBndr (AnnExpr AnnTerm)
               | ILet  AnnBindGroup (AnnExpr AnnTerm)
               | ICase (AnnExpr AnnTerm) (AnnExpr AnnType) [AnnAlt]
               | ILit Literal
               | IVar AnnBndr
               | ICast (AnnExpr AnnTerm) AnnCo
               | IType (AnnExpr AnnType)
               | ICoercion AnnCo

data AnnType = IVarTy AnnBndr
  | IAppTy (AnnExpr AnnType) (AnnExpr AnnType)
  | ITyConApp TyCon [AnnExpr AnnType]
  | IForAllTy AnnBndr (AnnExpr AnnType)
  | IFunTy (AnnExpr AnnType) (AnnExpr AnnType)
  | ILitTy TyLit
  | ICastTy (AnnExpr AnnType) AnnCo
  | ICoercionTy AnnCo

data AnnKind = Star

type family AssociatedCore a where
  AssociatedCore AnnTerm = CoreExpr
  AssociatedCore AnnType = Type
  AssociatedCore AnnKind = ()

type family TypeLift a where
  TypeLift AnnTerm = AnnType
  TypeLift AnnType = AnnKind 
  TypeLift AnnKind = AnnKind

data AnnExpr expr = MkAE
  { ae_expr :: (expr, AssociatedCore expr),
    ae_type :: AnnExpr (TypeLift expr),
    ae_depth :: Int,
    ae_showTypeInfo :: Bool,
    ae_parens :: Bool
  }

starKind :: AnnExpr AnnKind
starKind = MkAE {
  ae_expr = (Star, ()),
  ae_type = starKind,
  ae_depth = 0,
  ae_showTypeInfo = False,
  ae_parens = False
}

annotateModule :: CoreModule -> AnnModule
annotateModule m@(CoreModule mod _ bindings _) =
    let (binds, specialBinds) = seperateBindings.fmap annotateBindGroup $ bindings
      in MkAM m (modName mod) binds specialBinds

seperateBindings :: [AnnBindGroup] -> ([AnnBindGroup], [AnnBindGroup])
seperateBindings = partitionWith f
  where
    f :: AnnBindGroup -> Either AnnBindGroup AnnBindGroup
    f cb = case cb of
      ABNonRec bind -> case T.unpack . ab_name . ab_bndr $ bind of
        '$' : _ -> Right cb
        _ -> Left cb
      ABRec (bind : _) -> case T.unpack . ab_name . ab_bndr $ bind of
        '$' : _ -> Right cb
        _ -> Left cb
      _ -> error "Unreacheable state reached"

annotateBindGroup :: CoreBind -> AnnBindGroup
annotateBindGroup = \case
  NonRec var ex -> ABNonRec $ annotateBind (var, ex)
  Rec bs -> ABRec $ fmap annotateBind bs

annotateBind :: (CoreBndr, Expr CoreBndr) -> AnnBind
annotateBind a@(var, expr) = MkAB
            (annotateBndr var False)
            (annotateExpr expr)

annotateBndr :: CoreBndr -> Bool -> AnnBndr
annotateBndr b =
  let name = textOccName b 
  in if isId b 
    then TermBndr name (annotateType.varType$b)
    else TypeBndr name (annotateType.tyVarKind$b)

annotateAlt :: Alt CoreBndr -> AnnAlt
annotateAlt (altCon, binds, expr) = MkAA (altCon, (flip annotateBndr) True <$> binds, annotateExpr expr)

annotateExpr :: CoreExpr -> AnnExpr AnnTerm
annotateExpr e = let eTy = annotateType . CoreUtils.exprType $ e
                     base = \ae -> MkAE (ae, e) eTy 0 False False
                  in case e of
                   Var v -> base $ IVar $ annotateBndr v False
                   Lit l -> base $ ILit l
                   App e1 e2 -> let ae1 = annotateExpr e1
                                    ae2 = annotateExpr e2
                                    ae1' = case ae1 of
                                             MkAE (IApp _ _, _) _ _ _ _ -> ae1 {ae_parens = False}
                                             _ -> ae1
                    in MkAE (IApp ae1' ae2 ,e) eTy (ae_depth ae1 + ae_depth ae2) False True
                   Lam b exp -> let sub = annotateExpr exp
                    in MkAE (ILam (annotateBndr b True) sub, e) eTy (1 + ae_depth sub) False False
                   Let b exp -> let subB = annotateBindGroup b
                                    subE = annotateExpr exp
                    in MkAE (ILet subB subE, e) eTy (1 + ae_depth subE + abg_depth subB) False True
                   Case ex1 _ ty alts -> let subE1 = annotateExpr ex1
                                             annTy = annotateType ty
                                             annAlts  = fmap annotateAlt alts
                    in MkAE (ICase subE1 annTy annAlts,e) eTy (1 + ae_depth subE1) False False
                   Cast ex co -> let annEx = annotateExpr ex
                    in MkAE (ICast annEx co ,e) eTy (1 + ae_depth annEx) False False
                   Tick _ e1 -> annotateExpr e1
                   Type t1 -> let t1' = annotateType t1
                    in MkAE (IType t1', e) eTy (1 + ae_depth t1') False False
                   Coercion c -> base $ ICoercion c

annotateType :: Type -> AnnExpr AnnType
annotateType t =
  let tTy  = starKind
      base = \at -> MkAE (at, t) tTy 0 False False
  in case t of
    TyVarTy var -> base $ IVarTy $ annotateBndr var False
    AppTy t1 t2 -> let at1 = annotateType t1
                       at2 = annotateType t2
                       at1' = case at1 of
                                MkAE (IAppTy _ _, _) _ _ _ _ -> at1 {ae_parens = False}
                                _ -> at1
        in MkAE (IAppTy at1' at2 ,t) tTy (ae_depth at1 + ae_depth at2) False True
    TyConApp t1 ts -> let ats = fmap annotateType ts
        in MkAE (ITyConApp t1  ats, t) tTy (foldr (\a b -> b + ae_depth a) 1 ats) False (length ts > 1)
    ForAllTy (Bndr var _) t1 -> let at1 = annotateType t1
                                    bndr = annotateBndr var True
        in MkAE (IForAllTy bndr at1,t) tTy (ae_depth at1) False False
    FunTy _ t1 t2 -> let at1 = annotateType t1
                         at2 = annotateType t2
                         at1' = case at1 of
                                MkAE (IFunTy _ _, _) _ _ _ _ -> at1 {ae_parens = True}
                                _ -> at1
        in MkAE (IFunTy at1' at2, t) tTy (1 + ae_depth at1 + ae_depth at2) False False
    LitTy lit -> base $ ILitTy lit
    CastTy t1 co -> let at1 = annotateType t1
                    in MkAE (ICastTy at1 co,t) tTy (ae_depth at1) False False
    CoercionTy co -> base $ ICoercionTy co

modName :: Module -> T.Text
modName = T.pack . unpackFS . moduleNameFS . GHC.moduleName

textOccName :: HasOccName n => n -> T.Text
textOccName = T.pack . unpackFS . occNameFS . occName
