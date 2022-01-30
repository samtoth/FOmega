{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.FSyn where

import Data.Data (Proxy (Proxy))
import GHC.TypeLits hiding (Symbol)
import qualified Data.Text as T
import Data.Functor
import Data.Bifunctor

type Universe = Nat
type Lift :: Universe -> Universe
type Lift a = (a + 1)

type FTerm = 0
type FType = 1
type FKind = 2

type Sig :: * -> *
type family Sig a where
  Sig (FExpr a) = FSig (Lift a)

type FUName = T.Text

data FBndr e = MkBndr FUName (FSig (Lift e))
  deriving (Eq)


data FData = MkData -- TODO!
  deriving (Eq)
data FCaseArm a = MkArms -- TODO!
  deriving (Eq)
data FLiteral = MkLit -- TODO!
  deriving (Eq)
data FClassDef a = MkCD -- TODO!
  deriving (Eq)
data FInstanceHead a = MkIH -- TODO!
  deriving (Eq)

data FModule = MkFM {
  fm_name :: T.Text
, fm_bindings :: [FDeclaration FTerm]
}

 -- | Declaration is parameterised over universe
data FDeclaration :: Universe -> * where
  ValDecl :: Bool -- ^ Exported?
          -> FBinding a
          -> FDeclaration a
  DataDecl :: (1 <= a ) => FBndr a -> FData -> FDeclaration a
  ClassDecl :: FClassDef a -> FDeclaration a
  InstanceDef :: FInstanceHead a -> FDeclaration a

deriving instance Eq (FDeclaration n)


data FBinding a = MkBinding (FBndr a) (FExpr a)
  deriving (Eq)

data FExpr :: Universe -> * where
  Var :: FBndr a -> FExpr a
  App :: FExpr a -> FExpr a -> FExpr a
  Abs :: FBndr a -> FExpr a -> FExpr a
  Let :: [FBinding a] -> FExpr a -> FExpr a
  Case :: (k ~ FExpr a) => k -> Sig k -> [FCaseArm a] -> FExpr a
  TypeApp :: FExpr a -> FExpr (Lift a) -> FExpr a
  TypeAbs :: FBndr (Lift a) -> FExpr a -> FExpr a
  Lit :: FLiteral -> FExpr a
  
deriving instance Eq (FExpr a)

data FSig :: Universe -> * where
  SVar :: FBndr a -> FSig a
  SMap :: FSig a -> FSig a -> FSig a
  SForall :: FBndr a -> FSig a -> FSig a
  SApp :: FSig a -> FSig a -> FSig a
  SLit :: FLiteral -> FSig a
  deriving (Eq)

type FError = String

typeof :: FExpr n -> Either FError (FSig (n+1))
typeof = \case
  Var (MkBndr _ s) -> Right s
  App e1 e2 -> typeof e1 >>=
      (\case
        SMap s1 s2 -> typeof e2 >>= \se2 -> if se2 == s1 then Right s2 else Left "Expecting type fe(...) but actually got type fs'(...)"
        _ -> Left "Expecting a map type but found '...' ")
  Abs (MkBndr _ sig) fe -> typeof fe >>= \te -> Right $ SMap sig te
  Let _ fe -> typeof fe
  Case _ fs _ -> Right fs
  TypeApp e1 e2 -> do
    s1 <- typeof e1
    error "Need to figure out type application"
  TypeAbs fb fe -> typeof fe >>= \se -> Right $ SForall fb se 
  Lit fl -> error "Literals are TODO!"

--   fmap ::¹ * -> * -> (* -> *) -> *
--  fmap :: ∀a:: *, b :: *, f :: * -> *. (a -> b) -> f a -> f b -> f
--  myfunc = fmap @Int @Int @[] 