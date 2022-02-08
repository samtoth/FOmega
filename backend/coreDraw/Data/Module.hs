{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Data.Module where

import Control.Applicative
import Control.Monad.RWS (MonadWriter, tell)
import Control.Monad.Reader
import Data.Aeson (ToJSON, object, toJSON, (.=))
import qualified Data.Semigroup as Semigroup
import Data.Text as T
import Data.Text.Encoding as T
import qualified Data.Text.Encoding as BS
import Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding as LT
import Text.LaTeX (IsString, LaTeX, LaTeXT, Texy (texy), fromString, textell, runLaTeXT)
import Text.LaTeX.Base.Class

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

data ProcessedModule = MkMod
  { pm_name :: T.Text,
    pm_mainBinds :: [ProcessedBindgroup],
    pm_specialBinds :: [ProcessedBindgroup],
    pm_types :: ProcessedTypeEnv
  }
  deriving (Generic, Show)

instance ToJSON ProcessedBind

instance ToJSON ProcessedBindgroup

instance ToJSON ProcessedModule where
  toJSON MkMod {pm_name = n, pm_mainBinds = b, pm_specialBinds = sb, pm_types = t} =
    object
      [ "name" .= n,
        "main_binds" .= b,
        "special_binds" .= sb,
        "types" .= t
      ]

type ProcessedTypeEnv = ()

data ProcessedBindgroup
  = Binding ProcessedBind
  | RecursiveBinding [ProcessedBind]
  deriving (Generic, Show)

data ProcessedBind = MkProcessedBind
  { bindName :: T.Text,
    bindType :: T.Text,
    bindBody :: T.Text
  }
  deriving (Generic, Show)

data ModContext = MkModContext
  { mc_indentationLvl :: Int,
    mc_selected       :: Bool,
    mc_inParens       :: Bool
  }


inParens, noParens :: ModContext -> ModContext
inParens m = m {mc_inParens = True}
noParens m = m {mc_inParens = False}

{-
newtype SexT m a = MkSexT
  { runSexT :: LaTeXT (ReaderT ModContext m) a
  }
  deriving (Monad, Functor, Applicative)

instance MonadTrans SexT where
  lift = _

instance (Monad m) => MonadReader ModContext (SexT m) where
  ask = lift ask
  local = _

deriving instance (Monad m, Semigroup.Semigroup a) => Semigroup (SexT m a)

deriving instance (Monad m, Monoid a) => Monoid (SexT m a)

deriving instance (Monad m) => IsString (SexT m ())

deriving instance (Monad m) => LaTeXC (SexT m ())
-}