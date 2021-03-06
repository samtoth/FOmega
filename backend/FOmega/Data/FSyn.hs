{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE KindSignatures, AllowAmbiguousTypes #-}


{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}


module Data.FSyn where
  
import GHC.TypeLits hiding (Symbol)
import qualified Data.Text as T

import Data.Type.Bool

import Data.Singletons
import Data.Bool.Singletons
import Data.Kind

class DebugShow a where
  debugShow :: a -> T.Text

(...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
g ... f = \a b -> g (f a b)

type Universe = Nat
type Lift :: Universe -> Universe
type Lift a = (a + 1)

type FTerm = 0
type FType = 1
type FKind = 2

type Sig :: Type -> Type
type family Sig a where
  Sig (FExpr a n) = FExpr (Lift a) True

type FUName = T.Text

data FBndr e = MkBndr FUName (FExpr (Lift e) True)
--deriving instance Eq (FBndr e)

instance Eq (FBndr e) where
  (MkBndr n1 e1) == (MkBndr n2 e2) = n1 == n2 && e1 == e2


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

type CanSig = Bool

data Exported = Exported | Private

 -- | Declaration is parameterised over universe
data FDeclaration :: Universe -> Type where
  ValDecl :: Exported
          -> FBinding a
          -> FDeclaration a
  DataDecl :: (1 <= a ) => FBndr a -> FData -> FDeclaration a
  ClassDecl :: FClassDef a -> FDeclaration a
  InstanceDef :: FInstanceHead a -> FDeclaration a
--deriving instance Eq (FDeclaration n)


data FBinding a = forall n. MkBinding (FBndr a) (FExpr a n)
--deriving instance Eq (FBinding a)

data FExpr :: Universe
           -> CanSig  -- ^ Can the expression be used in the context of a type/kind siganture
           -> Type where
  Var :: FBndr a -> FExpr a True
  App :: (SingI j, SingI k) => FExpr a j -> FExpr a k -> FExpr a (j && k)  -- ^ Needed to use singletons to fix 
  Abs :: FBndr a -> FExpr a n -> FExpr a False
  Let :: [FBinding a] -> FExpr a n -> FExpr a False
  Case :: (k ~ FExpr a n) => k -> Sig k -> [FCaseArm a] -> FExpr a False
  Lit :: FLiteral -> FExpr a True
  Map :: (1 <= n) => FExpr n True -> FExpr n True -> FExpr n True
  Forall :: (1 <= n) => FBndr n -> FExpr n True -> FExpr n True
  Star :: (2 <= n) => FExpr n True

extractSigApp :: forall a b n. (SingI a, SingI b, (a && b) ~ True)
  => FExpr n a
  -> FExpr n b
  -> (FExpr n True, FExpr n True)
extractSigApp x y = case (sing @a, sing @b) of {(STrue, STrue) -> (x, y)}

extractAppExpr :: ((a && b) ~ 'True, SingI a, SingI b) => FExpr n a -> FExpr n b -> FExpr n 'True
extractAppExpr = fst...extractSigApp

extractAppArg :: ((a && b) ~ 'True, SingI a, SingI b) => FExpr n a -> FExpr n b -> FExpr n 'True
extractAppArg = snd...extractSigApp

instance Eq (FExpr n True) where
  (Var bndr1) == (Var bdnr2) = bndr1 == bdnr2
  (App e11' e12') == (App e21' e22') 
              =   let (e11, e12) = extractSigApp e11' e12'
                      (e21, e22) = extractSigApp e21' e22'
                  in e11 == e21 && e12 == e22
  (Lit l1) == (Lit l2) = l1 == l2
  (Map e11 e12) == (Map e21 e22) = e11 == e21 && e12 == e22
  (Forall bndr1 e1) == (Forall bndr2 e2) = bndr1 == bndr2 && e1 == e2
  Star == Star = True
  _ == _ = False

type FError = String

data FTyError n = forall k j i. AppMismachedArgs 
                    (Sig (FExpr n k))  -- ^ Expected type
                    (FExpr n j, Sig (FExpr n i)) -- ^ recieved expr and corresponding type
              |   forall k j i. ExpectingFnTy
                    (Maybe (Sig (FExpr n k))) -- ^ Expected type if more specific type info is available
                    (FExpr n j, Sig (FExpr n i)) -- ^ recieved expr and corresponding type 
              |   forall k j i. TyAppMismachedArgs
                    (FExpr (n+2) k)  -- ^ Expected kind
                    (FExpr (Lift n) j , Sig (FExpr (Lift n) i)) -- ^ recieved type and corresponding kind
              |   forall k j i. ExpectingForallKind
                    (Maybe (Sig (FExpr n k))) -- ^ Expected kind if more specific kind info is available
                    (FExpr n j, Sig (FExpr n i)) -- ^ recieved kind and corresponding kind 

instance DebugShow (FExpr n k) where
  debugShow = const "expr(...)"

instance DebugShow (FTyError n) where
  debugShow = \case
    AppMismachedArgs expected (recE, recT) -> "Expecting type " <> debugShow expected <> " but actually got expr " <> debugShow recE
                                           <> ". \n\tof type: " <> debugShow recT
    ExpectingFnTy _ (recE, recT) -> "Expecting an expression of type (a -> b) type but found " <> debugShow recE <> " of type " <> debugShow recT
    _ -> error "TODO"

sigof :: (expr ~ FExpr n k) => expr -> Either (FTyError n) (Sig expr)
sigof = \case
  Var (MkBndr _ s) -> Right s
  App e1 e2 -> sigof e1 >>= \case
        Map s1 s2 -> sigof e2 >>= \se2 -> if se2 == s1 then Right s2 else Left $ AppMismachedArgs s1 (e2, se2)
        t -> Left $ ExpectingFnTy Nothing (e1, t)
  Abs (MkBndr _ sig) fe -> do
    te <- sigof fe
    Right $ Map sig te
  Let _ fe -> sigof fe
  Case _ fs _ -> Right fs
  Lit fl -> error "Literals are TODO!"
  Forall b e -> error ""
  Star -> Right Star
  Map _ _ -> Right Star

--   fmap ::?? * -> * -> (* -> *) -> *
--  fmap :: ???a:: *, b :: *, f :: * -> *. (a -> b) -> f a -> f b -> f
--  myfunc = fmap @Int @Int @[] 