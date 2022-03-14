{-# LANGUAGE RankNTypes, GADTs, ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib where

    import Data.Text as T

    type Erased = Bool

    data FormCore where
        All :: Erased -> T.Text -> FormCore -> FormCore -> FormCore
        Lam :: T.Text -> FormCore -> FormCore -> FormCore
        App :: FormCore -> FormCore -> FormCore
        Var :: T.Text -> FormCore
        Set :: FormCore


    formId :: FormCore
    formId 
      = Lam "α" Set (
            Lam "a" (Var "α") (
                Var "a"
            )
        )


    instance Treeable a => Treeable [a] where
        name n = "parent"
        children = fmap MkTreeItem

    instance Treeable T.Text where
        name = id
        children = const mempty

    instance Treeable FormCore where
        name (All True txt fc fc') = T.pack $ "∀ { " ++ T.unpack txt ++ " }"
        name (All False txt fc fc') = T.pack $ "∀ ( " ++ T.unpack txt ++ " )"
        name (Lam txt fc fc') = T.pack $ "λ " ++ T.unpack txt
        name (App fc fc') = "$"
        name (Var txt) = T.pack $ "@ " ++ T.unpack txt
        name Set = "Set"

        children (All _ _ fc fc') = MkTreeItem <$> [fc, fc']
        children (Lam _ fc fc') = MkTreeItem <$> [fc, fc']
        children (App fc fc') = MkTreeItem <$> [fc, fc']
        children _ = []

    data TreeItem = forall a. Treeable a => MkTreeItem a

    instance Treeable TreeItem where
        name (MkTreeItem a) = name a
        children (MkTreeItem a) = children a

    class Treeable a where
        name :: a -> T.Text
        children :: a -> [TreeItem]



{----------------------------------------------------------------
all 	@self(name: xtyp) rtyp 	function type
all 	%self(name: xtyp) rtyp 	like all, erased before compile
lam 	#var body 	pure, curried, anonymous, function
app 	(func argm) 	applies a lam to an argument
let 	!x = expr; body 	local definition
def 	$x = expr; body 	like let, erased before check/compile
ann 	{term : type} 	inline type annotation
nat 	+decimal 	natural number, unrolls to lambdas
chr 	'x' 	UTF-16 character, unrolls to lambdas
str 	"content" 	UTF-16 string, unrolls to lambdas

-}