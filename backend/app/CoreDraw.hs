{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fdefer-typed-holes #-}

module CoreDraw (drawModule) where

import CoreSyn
import Data.ByteString as BS
import Data.ByteString.Lazy as LB
import Data.String (IsString)
import Data.Text as T
import Data.Text.Encoding as T
import qualified Data.Text.Encoding as BS
import Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding as LT
import FastString
import GHC
import Literal
import Module
import OccName (HasOccName (occName), OccName (occNameFS))
import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSFonts (mathbb)
import Text.LaTeX.Packages.AMSMath (align, cases, medspace, space)
import Text.LaTeX.Packages.Relsize (textlarger)
import TyCoRep
import TyCon
import Var
import Data.Coerce (coerce)
import Text.LaTeX.Packages.AMSSymb (vartriangleright)

renderTex :: LaTeX -> T.Text
renderTex = render

drawModule :: CoreModule -> T.Text
drawModule = renderTex . texy

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
  texy (NonRec bind expr) = mathrm (texy $textOccName bind) & "=" & texy expr <> lnbk
  texy (Rec binds) = multicolumn 3 [LeftColumn] (array Nothing [VerticalLine, RightColumn, CenterColumn, LeftColumn] (mconcat . fmap f $ binds)) <> lnbk
    where
      f (bind, expr) = mathrm (texy $ textOccName bind) & "=" & texy expr <> lnbk

instance Texy CoreExpr where
  texy = \case
    Var bind -> texy bind
    Lit lit -> texy lit
    App expr arg -> autoParens (texy expr <> space <> texy arg)
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
    Let bind expr -> "Let " <> texy bind <> qquad <> "in " <> texy expr
    Type ty -> texy ty
    Case expr bind ty arms ->
      "Case" !: texy ty <> quad <> texy bind <> "of" <> lnbk
        <> raw "&&"
        <> cases (mconcat . fmap ((<> lnbk) . texy . MkAlt) $ arms)
    Cast expr coerc -> texy expr <> vartriangleright <> texy coerc
    Tick _ expr -> texy expr
    Coercion co -> texy co

instance Texy Coercion where
  texy = \case
    Refl ty -> textit "Refl " <> texy ty
    GRefl ro ty mc -> _
    TyConAppCo ro tc cos -> _
    AppCo co co' -> _
    ForAllCo var co co' -> _
    FunCo ro co co' -> _
    CoVarCo var -> texy var
    AxiomInstCo ca n cos -> _
    AxiomRuleCo car cos -> _
    UnivCo ucp ro ty ty' -> _
    SymCo co -> _
    TransCo co co' -> _
    NthCo ro n co -> _
    LRCo lor co -> _
    InstCo co co' -> _
    KindCo co -> _
    SubCo co -> _
    HoleCo ch -> _


newtype AltNt = MkAlt (Alt CoreBndr)
instance Texy AltNt where
  texy (MkAlt (conType, binds, expr)) =
    texy conType
      <> medspace
      <> (mconcat . fmap ((<> medspace) . texy)) binds
      & rightarrow
      <> texy expr

instance Texy AltCon where
  texy = \case
    DataAlt dataCon -> texy . textOccName . getName $ dataCon
    LitAlt lit -> ""
    DEFAULT -> texttt "DEFAULT"

instance Texy Id where
  texy bind =
    let name = textOccName bind
        ty = varType bind
     in (if isTyVar bind then textit $ texy name else textrm $ texy name) ^: texy ty

instance Texy Literal where
  texy = \case
    LitChar c -> "'" <> texttt (texy c) <> "''"
    LitString s -> "\"" <> textit (texy . BS.decodeUtf8 $s) <> "\""
    LitNumber litTy int ty -> texy int
    LitFloat rat -> texy rat
    LitDouble rat -> texy rat
    other -> "unkown literal"

instance Texy Type where
  texy = \case
    TyVarTy var -> texy $ textOccName var
    AppTy t1 t2 -> autoParens (texy t1 <> space <> texy t2)
    TyConApp t ts -> texy t <> space <> (mconcat . fmap texy) ts
    ForAllTy bind t -> forall <> texy bind <> ". " <> texy t
    FunTy _ t1 t2 -> autoParens (texy t1 <> rightarrow <> texy t2)
    LitTy lit -> case lit of
      NumTyLit int -> mathbb $ texy int
      StrTyLit fs -> textbf $ "\"" <> texy fs <> "\""
    CastTy t co -> "castTy"
    CoercionTy coercion -> "coercionTy"

instance Texy TyCoVarBinder where
  texy (Bndr var arg) = texy var

instance Texy TyCon where
  texy con = let name = tyConName con in texy $ textOccName name

instance Texy FastString where
  texy = texy . unpackFS

modName :: Module -> T.Text
modName = T.pack . unpackFS . moduleNameFS . GHC.moduleName

textOccName :: HasOccName n => n -> T.Text
textOccName = T.pack . unpackFS . occNameFS . occName
