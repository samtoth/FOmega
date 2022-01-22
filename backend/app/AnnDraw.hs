{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module AnnDraw where

import Data.AnnCore
import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSFonts (mathbb)
import Text.LaTeX.Packages.AMSMath (align, cases, medspace, space, text)
import Text.LaTeX.Packages.AMSSymb (vartriangleright)
import Text.LaTeX.Packages.Relsize (textlarger)
import TyCoRep
import Literal
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding as BS
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import FastString
import TyCon
import Var
import CoreSyn hiding (AnnExpr, AnnBind, AnnAlt)
import Name

instance Texy (AnnExpr AnnTerm) where
    texy (MkAE (term, _) ty depth showty parens)
      = (if showty then (^: texy ty) else id) $
        (if parens then autoParens else id)   $
          (case term of
              IVar bind -> texy bind
              ILit lit -> texy lit
              IApp expr arg -> texy expr <> space <> texy arg
              ILam bind expr -> (case bind of
                                TermBndr {}  -> lambda
                                TypeBndr {} -> lambdau)
                                <> texy bind
                                <> "."
                                <> quad
                                <> texy expr
              ILet bind expr -> lnbk <> "Let " <> quad <> texy bind <> lnbk <> "in " <> medspace <> texy expr
              IType ty -> texy ty
              ICase expr ty arms ->
                "Case" !: texy ty <> space <> texy expr <> space <> "of" <> lnbk
                    <> cases (mconcat . fmap ((<> lnbk) . texy) $ arms)
              ICast expr coerc -> texy expr <> vartriangleright <> const "Coercion" coerc
              ICoercion co -> const "Coercion" co
          )

instance Texy (AnnExpr AnnType) where
    texy (MkAE  (ty, _) k depth showty parens)
      = (if parens then autoParens else id)
          (case ty of
            IVarTy var -> texy var
            IAppTy ae ae' -> texy ae <> space <> texy ae'
            ITyConApp tc aes -> texy tc <> space <> (mconcat . fmap texy) aes
            IForAllTy vb ae -> forall <> texy vb <> ". " <> texy ae
            IFunTy ae ae' -> texy ae <> rightarrow <> texy ae'
            ILitTy tl -> case tl of
              NumTyLit int -> mathbb $ texy int
              StrTyLit fs -> textbf $ "\"" <> texy fs <> "\""
            ICastTy ae co -> "castTy"
            ICoercionTy co -> "coTy"
          )

instance Texy AnnBndr where
    texy = \case
      TermBndr n t s -> (if s then (^: texy t) else id) $ textrm (texy n)
      TypeBndr n t s -> (if s then (^: texy t) else id) $ textit (texy n)

instance Texy AnnBindGroup where
  texy = \case
    ABRec abs -> mconcat.fmap ((<> lnbk).texy) $ abs
    ABNonRec ab -> texy ab

instance Texy AnnBind where
  texy (MkAB ab ae) = texy ab <> space =: space <> texy ae

instance Texy AnnAlt where
  texy (MkAA (altCon, binds, expr)) = 
         texy altCon
      <> medspace
      <> (mconcat . fmap ((<> medspace) . texy)) binds
      &  rightarrow
      <> texy expr

instance Texy AltCon where
  texy = \case
    DataAlt dataCon -> texy . textOccName . getName $ dataCon
    LitAlt lit -> texy lit
    DEFAULT -> texttt "DEFAULT"


instance Texy Literal where
  texy = \case
    LitChar c -> "'" <> texttt (texy c) <> "''"
    LitString s -> "\"" <> textit (texy . BS.decodeUtf8 $s) <> "\""
    LitNumber litTy int ty -> texy int
    LitFloat rat -> texy rat
    LitDouble rat -> texy rat
    other -> "unkown literal"


instance Texy TyCon where
  texy con = let name = tyConName con in texy $ textOccName name

instance Texy FastString where
  texy = texy . unpackFS