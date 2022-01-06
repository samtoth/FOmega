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

module FSyn where

import Data.Data (Proxy (Proxy))
import GHC.TypeLits hiding (Symbol)

{-

 * Term :==  Abstraction (Ident, Type)  Term            `(λx:α. e)
           | Application Term           Term            `(x y    )
           | Reference   Ident                          `(x      )

 * Type :==  Abstraction (Ident, Kind)  Term            `(Λα:*. ε)
           | Application Type           Type            `(α β    )
           | Reference   Ident                          `(α      )

(->) type is representated as α:* -> * -> *

 *

-}

type Symbol = String

data AExpr
  = Var Symbol
  | App AExpr AExpr
  | Abs (Symbol, AType) AExpr
  | TypeApp AExpr AType
  | TypeAbs (Symbol, AKind 0) AExpr

data AType
  = TVar Symbol
  | Func AType AType
  | Forall (Symbol, AKind 0) AType
  | Exists (Symbol, AKind 0) AType
  | TApp AType AType
  | TAbs (Symbol, AKind 0) AType

data AKind :: Nat -> * where
  K :: AKind a
  KFunc :: AKind a -> AKind a -> AKind a
  KForall :: (Symbol, AKind (a + 1)) -> AKind a -> AKind a
  KExists :: (Symbol, AKind (a + 1)) -> AKind a -> AKind a
  KApp :: AKind a -> AKind a -> AKind a
  KAbs :: (Symbol, AKind (a + 1)) -> AKind a -> AKind a

class Format a where
  prettyP :: a -> String

instance Format AExpr where
  prettyP (Var s) = s
  prettyP (App e1 e2) = prettyP e1 ++ " " ++ prettyP e2
  prettyP (Abs (s, t) e) = "λ" ++ s ++ ":" ++ prettyP t ++ ". " ++ prettyP e
  prettyP (TypeApp e t) = prettyP e ++ " " ++ prettyP t
  prettyP (TypeAbs (s, k) e) = "Λ" ++ s ++ ":" ++ prettyP k ++ ". " ++ prettyP e

instance Format AType where
  prettyP (TVar s) = s
  prettyP (Func t1 t2) = prettyP t1 ++ " -> " ++ prettyP t2
  prettyP (Forall (s, k) t) = "∀" ++ s ++ ":" ++ prettyP k ++ ". " ++ prettyP t
  prettyP (Exists (s, k) t) = "∃" ++ s ++ ":" ++ prettyP k ++ ". " ++ prettyP t
  prettyP (TApp t1 t2) = prettyP t1 ++ " " ++ prettyP t2
  prettyP (TAbs (s, k) t) = "λ" ++ s ++ ":" ++ prettyP k ++ ". " ++ prettyP t

instance Format (AKind 0) where
  prettyP K = "★"
  prettyP (KFunc k1 k2) = prettyP k1 ++ " ⇒ " ++ prettyP k2

instance Format (AKind 1) where
  prettyP (KFunc k1 k2) = prettyP k1 ++ " ⇒¹ " ++ prettyP k2
  prettyP _ = "★¹"

class ULevel a where
  type Lifted a
  lift :: a -> Lifted a

instance ULevel AExpr where
  type Lifted AExpr = AType

  lift (Var s) = TVar "_"
  lift (App e1 e2) = case lift e1 of
    Func t1 t2 -> t2
    _ -> error "Type check Error"
  lift (Abs (s, t) e) = Func t $ lift e
  lift (TypeApp e t) = t
  lift (TypeAbs (s, k) e) = Forall (s, k) $ lift e

instance ULevel AType where
  type Lifted AType = AKind 0

  lift (Forall (s, k) t) = KFunc k K

instance ULevel (AKind n) where
  type Lifted (AKind n) = AKind (n + 1)

  lift K = K
  lift (KFunc k1 k2) = K
  lift (KForall (s, kS) k) = KFunc kS K

exampleExpr = TypeAbs ("t", K) (Abs ("x", TVar "t") (Var "x"))

example = do
  putStrLn . ("  " ++) . prettyP $ exampleExpr
  putStrLn "has type"
  putStrLn . ("  " ++) . prettyP . lift $ exampleExpr
  putStrLn "has kind"
  putStrLn . ("  " ++) . prettyP . lift . lift $ exampleExpr
