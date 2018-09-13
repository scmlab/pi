module Syntax where

import Utilities

type Name = Int

type Prog = [(Name, Pi)]

data Pi = End
        | Send Expr Expr Pi
        | Recv Expr Ptrn Pi
        | Par Pi Pi
        | Nu Name Pi
        | Call Name
   deriving Show

data Expr = EV Val
          | EPlus Expr Expr
          | EMinus Expr Expr

          | EIf Expr Expr Expr

          | ETup [Expr]     -- n-tuples
          | EPrj Int Expr   -- projection of tuples
   deriving Show

data Val = N Name
         | VI Int
         | VB Bool
         | VT [Val]    -- n-tuples
   deriving (Eq, Show)

data Ptrn = PN Name         -- patterns
          | PT [Ptrn]
   deriving Show

eN :: Name -> Expr
eN = EV . N

eI :: Int -> Expr
eI = EV . VI

type Subst = FMap Name Val

-- substName :: Name -> Name -> Name -> Name
-- substName x z y | x == y    = z
--                 | otherwise = y

substVal :: Subst -> Val -> Val
substVal th (N y) | Just v <- lookup y th = v
                  | otherwise             = N y
substVal th (VT vs) = VT (map (substVal th) vs)
substVal th u = u

substExpr :: Subst -> Expr -> Expr
substExpr th (EV u) = EV (substVal th u)
substExpr th (EPlus e1 e2) =
  EPlus (substExpr th e1) (substExpr th e2)
substExpr th (EMinus e1 e2) =
  EMinus (substExpr th e1) (substExpr th e2)
substExpr th (EIf e0 e1 e2) =
  EIf (substExpr th e0) (substExpr th e1) (substExpr th e2)
substExpr th (ETup es) = ETup (map (substExpr th) es)
substExpr th (EPrj i e) = EPrj i (substExpr th e)

substPi :: Subst -> Pi -> Pi
substPi th End = End
substPi th (Send c u p) =
   Send (substExpr th c) (substExpr th u) (substPi th p)
substPi th (Recv c ys p) =
    if isEmpty th' then Recv c ys p
        else Recv (substExpr th' c) ys (substPi th' p)
  where th' = mask th ys
   -- | y `inDom` th = Recv c y p   -- is this right?
   -- | otherwise = Recv (substExpr th c) y (substPi th p)
substPi th (Par p q) = Par (substPi th p) (substPi th q)
substPi th (Nu y p)
   | y `inDom` th = Nu y p       -- is this right?
   | otherwise = Nu y (substPi th p)
substPi th (Call p) = Call p  -- perhaps this shouldn't be substituted?

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
evalExpr (ETup es) = VT (map evalExpr es)
evalExpr (EPrj i e)
  | VT vs <- evalExpr e = vs !! i
  | otherwise = error "type error in EPrj"

-- substitution related stuffs

match :: Ptrn -> Val -> Subst
match (PN x) v = [(x,v)]
match (PT xs) (VT vs) | length xs == length vs =
  concat (map (uncurry match) (zip xs vs))
match _ _ = error "pattern matching error"

mask :: Subst -> Ptrn -> Subst
mask th (PN x)  = rmEntry x th
mask th (PT []) = th
mask th (PT (p:ps)) = mask (mask th p) (PT ps)
