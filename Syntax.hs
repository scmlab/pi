{-#LANGUAGE FlexibleContexts #-}
module Syntax where

import Control.Monad.Except
import Data.List(nub)
import Utilities
import Control.Arrow ((***))

type Label = String
type ErrMsg = String
type RName = String   -- row name

data Name = NS RName   -- user defined
          | NG Int      -- system generated
          | NR ResName  -- reserved name
    deriving (Eq, Show)

data ResName = StdOut | StdIn
  deriving (Eq, Show)

type Prog = [(Name, Pi)]

data Pi = End
        | Send Expr Expr Pi
        | Recv Expr [(Ptrn,Pi)]
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
         | VL Label
   deriving (Eq, Show)

unVI :: MonadError ErrMsg m => Val -> m Int
unVI (VI n) = return n
unVI _ = throwError "type error: Int wanted"

unVB :: MonadError ErrMsg m => Val -> m Bool
unVB (VB b) = return b
unVB _ = throwError "type error: Bool wanted"

unVT :: MonadError ErrMsg m => Val -> m [Val]
unVT (VT xs) = return xs
unVT _ = throwError "type error: tuple wanted"

data Ptrn = PN Name         -- patterns
          | PT [Ptrn]
          | PL Label
   deriving Show

eN :: Name -> Expr
eN = EV . N

eI :: Int -> Expr
eI = EV . VI

eL :: Label -> Expr
eL = EV . VL

eR :: ResName -> Expr
eR = EV . N . NR

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
substPi th (Recv c pps) =
    if isEmpty th' then Recv c pps
        else Recv (substExpr th' c)
                  (map (id *** substPi th') pps)
  where th' = foldr mask th (map fst pps)
substPi th (Par p q) = Par (substPi th p) (substPi th q)
substPi th (Nu y p)
   | y `inDom` th = Nu y p       -- is this right?
   | otherwise = Nu y (substPi th p)
substPi th (Call p) = Call p  -- perhaps this shouldn't be substituted?

evalExpr :: MonadError ErrMsg m => Expr -> m Val
evalExpr (EV v) = return v
evalExpr (EPlus e1 e2) =
  VI <$> (liftM2 (+) (evalExpr e1 >>= unVI)
                     (evalExpr e2 >>= unVI))
evalExpr (EMinus e1 e2) =
  VI <$> (liftM2 (-) (evalExpr e1 >>= unVI)
                     (evalExpr e2 >>= unVI))
evalExpr (EIf e0 e1 e2) =
  (evalExpr e0 >>= unVB) >>= \v0 ->
  if v0 then evalExpr e1 else evalExpr e2
evalExpr (ETup es) = VT <$> mapM evalExpr es
evalExpr (EPrj i e) =
  (!! i) <$> (evalExpr e >>= unVT)

-- substitution related stuffs

match :: Ptrn -> Val -> Maybe Subst
match (PN x) v = Just [(x,v)]
match (PL x) (VL y) | x == y = Just []
match (PT xs) (VT vs) | length xs == length vs =
  joinSubs <$> mapM (uncurry match) (zip xs vs)
match _ _ = Nothing

joinSubs :: [Subst] -> Subst
joinSubs ss | nodup (map fst s) = s
            | otherwise = error "non-linear pattern"
  where s = concat ss

matchPPs :: [(Ptrn, Pi)] -> Val -> Maybe (Subst, Pi)
matchPPs [] v = Nothing
matchPPs ((pt,e):pps) v
  | Just ss <- match pt v = Just (ss,e)
matchPPs (_:pps) v = matchPPs pps v

mask :: Ptrn -> Subst -> Subst
mask (PN x)      = rmEntry x
mask (PT [])     = id
mask (PT (p:ps)) = mask (PT ps) . mask p
mask (PL _)      = id
