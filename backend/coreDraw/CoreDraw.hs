{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fdefer-typed-holes #-}

module CoreDraw (drawAnnModule, drawModule, ProcessedModule) where


import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.ByteString as BS
import Data.ByteString.Lazy as LB
import Data.Coerce (coerce)
import Data.String (IsString)
import Data.Text as T
import Data.Text.Encoding as T
import qualified Data.Text.Encoding as BS
import Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding as LT
import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSFonts (mathbb)
import Text.LaTeX.Packages.AMSMath (align, cases, medspace, space)
import Text.LaTeX.Packages.AMSSymb (vartriangleright)
import Text.LaTeX.Packages.Relsize (textlarger)
import Data.Module
import Data.AnnCore
import Control.Monad.Reader
import AnnDraw

import GHC.Core hiding (AnnBind, AnnExpr, AnnType, AnnAlt)
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon
import GHC.Core.Utils as CoreUtils
import GHC.Types.Literal
import GHC.Types.Var
import GHC.Types.Name.Occurrence
import GHC.Data.FastString
import GHC
import GHC.Unit.Module.Name
import GHC.Generics
import GHC.Utils.Misc (partitionWith)

renderTex :: LaTeX -> T.Text
renderTex = render

drawModule :: CoreModule -> ProcessedModule
drawModule
  CoreModule
    { cm_module = mod,
      cm_types = tyEnv,
      cm_binds = prog
    } =
    let name = modName mod
        (binds, specialBinds) = processBindings prog
     in MkMod
          { pm_name = name,
            pm_mainBinds = binds,
            pm_specialBinds = specialBinds,
            pm_types = const () tyEnv
          }

processBindings :: CoreProgram -> ([ProcessedBindgroup], [ProcessedBindgroup])
processBindings = partitionWith f
  where
    f :: CoreBind -> Either ProcessedBindgroup ProcessedBindgroup
    f = \case
      NonRec bind expr -> case T.unpack . textOccName $ bind of
        '$' : _ -> Right . Binding $ processBinding bind expr
        _ -> Left . Binding $ processBinding bind expr
      Rec binds@((b, _) : _) -> case T.unpack . textOccName $ b of
        '$' : _ -> Right . RecursiveBinding . fmap (uncurry processBinding) $ binds
        _ -> Left . RecursiveBinding . fmap (uncurry processBinding) $ binds
      _ -> error "Unreacheable state reached"

processBinding :: CoreBndr -> CoreExpr -> ProcessedBind
processBinding b e =
  MkProcessedBind
    { bindName = textOccName b,
      bindType = renderTex . texy . CoreUtils.exprType $ e,
      bindBody = renderTex . texy $ e
    }


drawAnnModule :: CoreModule -> ProcessedModule
drawAnnModule = f.annotateModule
    where f :: AnnModule -> ProcessedModule
          f (MkAM _ n b sb) = MkMod {
            pm_name = n,
            pm_mainBinds = processAnnBindGroup <$> b,
            pm_specialBinds = processAnnBindGroup <$> sb,
            pm_types = ()
          }

processAnnBindGroup :: AnnBindGroup -> ProcessedBindgroup
processAnnBindGroup = \case
  ABNonRec bind -> Binding $ processAnnBinding bind
  ABRec binds   -> RecursiveBinding . fmap processAnnBinding $ binds

processAnnBinding :: AnnBind -> ProcessedBind
processAnnBinding (MkAB bndr expr) = MkProcessedBind {
  bindName = ab_name bndr,
  bindType = renderTex . texy . ae_type $ expr,
  bindBody = renderTex . texy $ expr
}

instance Texy CoreModule where
  texy
    CoreModule
      { cm_module = mod,
        cm_types = tyEnv,
        cm_binds = prog
      } =
      array
        Nothing
        [RightColumn, CenterColumn, LeftColumn]
        ( mempty & (textbf . texy . modName) mod & lnbk
            <> mconcat (fmap texy prog)
        )

instance Texy CoreBind where
  texy (NonRec bind expr) = mathrm (texy $ textOccName bind) <> "=" <> texy expr <> lnbk
  texy (Rec binds) = multicolumn 3 [LeftColumn] (array Nothing [VerticalLine, RightColumn, CenterColumn, LeftColumn] (mconcat . fmap f $ binds)) <> lnbk
    where
      f (bind, expr) = mathrm (texy $ textOccName bind) <> "=" <> texy expr <> lnbk

instance Texy CoreExpr where
  texy = \case
    Var bind -> texy bind
    Lit lit -> texy lit
    App expr arg -> (case expr of {App _ _' -> id ; _ -> autoParens}) (texy expr <> space <> texy arg)
    Lam bind expr ->
      ( if isId bind
          then lambda
          else {- isTyVar = not isId -}
            lambdau
      )
        <> texy bind
        <> "."
        <> quad
        <> texy expr
    Let bind expr -> "Let " <> quad <> texy bind <> quad <> "in " <> texy expr
    Type ty -> texy ty
    Case expr bind ty arms ->
      "Case" !: texy ty <> quad <> texy bind <> "of" <> lnbk
        <> raw "&&"
        <> cases (mconcat . fmap ((<> lnbk) . texy ) $ arms)
    Cast expr coerc -> texy expr <> vartriangleright <> texy coerc
    Tick _ expr -> texy expr
    Coercion co -> texy co

instance Texy Coercion where
  texy = \case
    Refl ty -> textit "Refl " <> texy ty
    GRefl ro ty mc -> "GRefl"
    TyConAppCo ro tc cos -> "TyConAppCo"
    AppCo co co' -> "AppCo"
    ForAllCo var co co' -> forall <> texy var <> ". " <> quad <> "Coercion"
    FunCo ro mult co co' -> "FunCo"
    CoVarCo var -> texy var
    AxiomInstCo ca n cos -> "AxiomInstCo"
    AxiomRuleCo car cos -> "AxiomRuleCo"
    UnivCo ucp ro ty ty' -> "UnivCo"
    SymCo co -> "SymCo"
    TransCo co co' -> "TransCo"
    NthCo ro n co -> "NthCo"
    LRCo lor co -> "LRCo"
    InstCo co co' -> "InstCo"
    KindCo co -> "KindCo"
    SubCo co -> "SubCo"
    HoleCo ch -> "HoleCo"


instance Texy (Alt CoreBndr) where
  texy (Alt conType binds expr) =
    texy conType
      <> medspace
      <> (mconcat . fmap ((<> medspace) . texy)) binds
      & rightarrow
      <> texy expr


instance Texy TyCoVarBinder where
  texy (Bndr var arg) = texy var

instance Texy Id where
  texy bind =
    let name = textOccName bind
        ty = varType bind
     in (if isTyVar bind then textit $ texy name else textrm $ texy name) ^: texy ty



instance Texy Type where
  texy = \case
    TyVarTy var -> texy $ textOccName var
    AppTy t1 t2 -> (case t1 of {AppTy _ _ -> id ; _ -> autoParens }) (texy t1 <> space <> texy t2)
    TyConApp t ts -> texy t <> space <> (mconcat . fmap texy) ts
    ForAllTy bind t -> forall <> texy bind <> ". " <> texy t
    FunTy _ _ t1 t2 -> autoParens (texy t1 <> rightarrow <> texy t2)
    LitTy lit -> case lit of
      NumTyLit int -> mathbb $ texy int
      StrTyLit fs -> textbf $ "\"" <> texy fs <> "\""
    CastTy t co -> "castTy"
    CoercionTy coercion -> "coercionTy"



