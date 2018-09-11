module Syntax where

type Name = Int

data Val = N Name
         | VI Int
         | VB Bool
         | VP Val Val
   deriving (Eq, Show)

data Expr = EV Val
          | EPlus Expr Expr
          | EMinus Expr Expr

          | EIf Expr Expr Expr

          | EFst Expr
          | ESnd Expr
   deriving Show

eN :: Name -> Expr
eN = EV . N

eI :: Int -> Expr
eI = EV . VI

data Pi = End
        | Send Expr Expr Pi
        | Recv Expr Name Pi
        | Par Pi Pi
        | Nu Name Pi
   deriving Show

substName :: Name -> Name -> Name -> Name
substName x z y | x == y    = z
                | otherwise = y

substVal :: Name -> Val -> Val -> Val
substVal x v (N y) | x == y    = v
                   | otherwise = N y
substVal x v (VP l r) = VP (substVal x v l) (substVal x v r)
substVal x v u = u

substExpr :: Name -> Val -> Expr -> Expr
substExpr x v (EV u) = EV (substVal x v u)
substExpr x v (EPlus e1 e2) =
  EPlus (substExpr x v e1) (substExpr x v e2)
substExpr x v (EMinus e1 e2) =
  EMinus (substExpr x v e1) (substExpr x v e2)
substExpr x v (EIf e0 e1 e2) =
  EIf (substExpr x v e0) (substExpr x v e1) (substExpr x v e2)
substExpr x v (EFst e) = EFst (substExpr x v e)
substExpr x v (ESnd e) = ESnd (substExpr x v e)

substPi :: Name -> Val -> Pi -> Pi
substPi x v End = End
substPi x v (Send c u p) =
   Send (substExpr x v c) (substExpr x v u) (substPi x v p)
substPi x v (Recv c y p)
   | x == y = Recv c y p   -- is this right?
   | otherwise = Recv (substExpr x v c) y (substPi x v p)
substExp x v (Par p q) = Par (substPi x v p) (substPi x v q)
substExp x v (Nu y p)
   | x == y = Nu y p       -- is this right?
   | otherwise = Nu y (substPi x v p)

evalExpr :: Expr -> Val
evalExpr (EV v) = v
evalExpr (EPlus e1 e2)
  | VI v1 <- evalExpr e1,
    VI v2 <- evalExpr e2  = VI (v1 + v2)
  | otherwise = error "type error in EPlus"
evalExpr (EMinus e1 e2)
  | VI v1 <- evalExpr e1,
    VI v2 <- evalExpr e2  = VI (v1 - v2)
  | otherwise = error "type error in EMinus"
evalExpr (EIf e0 e1 e2)
  | VB v0 <- evalExpr e0 =
    if v0 then evalExpr e1 else evalExpr e2
  | otherwise = error "type error in EIf"
evalExpr (EFst e)
  | VP v1 v2 <- evalExpr e = v1
  | otherwise = error "type error in EFst"
evalExpr (ESnd e)
  | VP v1 v2 <- evalExpr e = v2
  | otherwise = error "type error in ESnd"
