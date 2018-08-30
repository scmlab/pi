module Syntax where

type Name = Int

data Val = N Name
   deriving (Eq, Show)

data Expr = End
          | Send Val Val Expr
          | Recv Val Name Expr
          | Par Expr Expr
          | Nu Name Expr
   deriving Show

substVal :: Name -> Val -> Val -> Val
substVal x v (N y)
  | x == y = v
  | otherwise = N y

substExp :: Name -> Val -> Expr -> Expr
substExp x v End = End
substExp x v (Send c u p) =
   Send (substVal x v c) (substVal x v u) (substExp x v p)
substExp x v (Recv c y p)
   | x == y = Recv c y p   -- is this right?
   | otherwise = Recv (substVal x v c) y (substExp x v p)
substExp x v (Par p q) = Par (substExp x v p) (substExp x v q)
substExp x v (Nu y p)
   | x == y = Nu y p       -- is this right?
   | otherwise = Nu y (substExp x v p)
