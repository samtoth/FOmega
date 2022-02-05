{-# LANGUAGE GADTs #-}

module GADTs where

    data Expr a where
        Zero :: Expr Int
        Succ :: Expr Int -> Expr Int
        Pair :: Expr a -> Expr b -> Expr (a, b)

    eval :: Expr a -> a
    eval Zero = 0
    eval (Succ n) = eval n + 1
    eval (Pair a b) = (eval a, eval b)

    