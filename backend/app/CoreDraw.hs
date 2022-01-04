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
import Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding as LT
import FastString
import GHC
import Module
import OccName (HasOccName (occName), OccName (occNameFS))
import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath (align, cases)
import Text.LaTeX.Packages.Relsize (textlarger)

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
  texy (Rec binds) = multicolumn 3 [RightColumn] (array Nothing [VerticalLine, RightColumn, CenterColumn, LeftColumn] (mconcat . fmap f $ binds)) <> lnbk
    where
      f (bind, expr) = mathrm (texy $textOccName bind) & "=" & texy expr <> lnbk

instance Texy CoreExpr where
  texy = \case

modName :: Module -> T.Text
modName = T.pack . unpackFS . moduleNameFS . GHC.moduleName

textOccName :: HasOccName n => n -> T.Text
textOccName = T.pack . unpackFS . occNameFS . occName
