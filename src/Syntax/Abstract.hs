{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Syntax.Abstract where

import Control.Monad.Except
import Data.Text (Text, pack)

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Type

-- type Label = Text  -- moved to Type
type ErrMsg = String
type ProcName = Text
type TypeName = Text

data Polarised a = Pos a | Neg a
    deriving (Ord, Eq, Show)
type Name = Polarised Text
data Chan = ND Name             -- user defined
          | NG (Polarised Int)  -- system generated
          | NR ResName          -- reserved name
    deriving (Ord, Eq, Show)

data PureName = PH Text        -- "pure" names, without polarization
              | PG Int
   deriving (Eq, Show, Ord)


instance HasDual (Polarised a) where
  dual (Pos a) = Neg a
  dual (Neg a) = Pos a

isPositive :: Polarised a -> Bool
isPositive (Pos _) = True
isPositive (Neg _) = False

depolarise :: Polarised a -> a
depolarise (Pos x) = x
depolarise (Neg x) = x
-- depolar (Neu x) = x

depolarCh :: Chan -> PureName
depolarCh (ND c) = PH (depolarise c)
depolarCh (NG c) = PG (depolarise c)
depolarCh (NR _) = error "bug: shouldn't call depolar without checking"

depolarCH :: Chan -> Text
depolarCH = depolarise . unND

unND :: Chan -> Name
unND (ND n) = n
unND _ = undefined

data ResName = StdOut | StdIn
  deriving (Ord, Eq, Show)

data Program = Program [Definition]
  deriving (Eq, Show)
data Definition = ProcDefn ProcName Proc
                | ChanType Chan Type
                | TypeDefn TypeName Type
  deriving (Eq, Show)

data Proc = End
          | Send Chan Expr Proc
          | Recv Chan [Clause]
          | Par Proc Proc
          | Nu Text (Maybe Type) Proc
          | Repl Proc
          | Call ProcName
   deriving (Eq, Show)

data Clause = Clause Ptrn Proc
   deriving (Eq, Show)

data Expr = EV Val
          | EAdd Expr Expr
          | ESub Expr Expr
          | EMul Expr Expr
          | EDiv Expr Expr
          -- boolean stuff
          | EEQ   Expr Expr
          | ENEQ  Expr Expr
          | EGT   Expr Expr
          | EGTE  Expr Expr
          | ELT   Expr Expr
          | ELTE  Expr Expr
          | EIf Expr Expr Expr
          -- tuples & projection of tuples
          | ETup [Expr]
          | EPrj Int Expr
   deriving (Eq, Show)

data Val = VC Chan
         | VI Int
         | VB Bool
         | VT [Val]    -- n-tuples
         | VL Label
         | VS Text
   deriving (Eq, Show)

data Ptrn = PtrnVar   Text         -- patterns
          | PtrnTuple [Ptrn]
          | PtrnLabel Label
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

ePtrnVar :: String -> Expr
ePtrnVar = EV . VC . ND . Pos . pack

eNN :: String -> Expr
eNN = EV . VC . ND . Neg . pack

-- eN :: String -> Expr
-- eN  = EV . VC . ND . Neu . pack

eI :: Int -> Expr
eI = EV . VI

eB :: Bool -> Expr
eB = EV . VB

eL :: String -> Expr
eL = EV . VL . pack

eR :: ResName -> Expr
eR = EV . VC . NR

cP :: String -> Chan
cP = ND . Pos . pack

cN :: String -> Chan
cN = ND . Neg . pack

par :: Proc -> Proc -> Proc
End `par` p = p
p `par` End = p
p `par` q = Par p q

type Subst = Map PureName Val

substChan :: Subst -> Chan -> Chan
substChan _ (NR r) = NR r
substChan th c =  case Map.lookup (depolarCh c) th of
  Just (VC y) -> y
  Just _ -> error "not a channel"
  Nothing -> c

substVal :: Subst -> Val -> Val
substVal _ (VC (NR r)) = VC (NR r)
substVal th (VC c) = case Map.lookup (depolarCh c) th of
  Just v  -> v
  Nothing -> VC c
substVal th (VT vs) = VT (map (substVal th) vs)
substVal _ u = u

substExpr :: Subst -> Expr -> Expr
substExpr th (EV u) = EV (substVal th u)
substExpr th (EAdd e1 e2) =
  EAdd (substExpr th e1) (substExpr th e2)
substExpr th (ESub e1 e2) =
  ESub (substExpr th e1) (substExpr th e2)
substExpr th (EMul e1 e2) =
  EMul (substExpr th e1) (substExpr th e2)
substExpr th (EDiv e1 e2) =
  EDiv (substExpr th e1) (substExpr th e2)
substExpr th (EEQ  e1 e2) = EEQ  (substExpr th e1) (substExpr th e2)
substExpr th (ENEQ e1 e2) = ENEQ (substExpr th e1) (substExpr th e2)
substExpr th (EGT  e1 e2) = EGT  (substExpr th e1) (substExpr th e2)
substExpr th (EGTE e1 e2) = EGTE (substExpr th e1) (substExpr th e2)
substExpr th (ELT  e1 e2) = ELT  (substExpr th e1) (substExpr th e2)
substExpr th (ELTE e1 e2) = ELTE (substExpr th e1) (substExpr th e2)
substExpr th (EIf e0 e1 e2) =
  EIf (substExpr th e0) (substExpr th e1) (substExpr th e2)
substExpr th (ETup es) = ETup (map (substExpr th) es)
substExpr th (EPrj i e) = EPrj i (substExpr th e)

substProc :: Subst -> Proc -> Proc
substProc _ End = End
substProc th (Send c u p) =
   Send (substChan th c) (substExpr th u) (substProc th p)
substProc th (Recv c clauses) =
    if Map.null th' then Recv c clauses
        else Recv (substChan th' c)
                  (map (\(Clause ptrn p) -> Clause ptrn (substProc th' p)) clauses)
  where th' = foldr mask th (map (\(Clause ptrn _) -> ptrn) clauses)
substProc th (Par p q) = Par (substProc th p) (substProc th q)
substProc th (Nu y t p) = if Map.member (PH y) th
   then Nu y t p       -- is this right?
   else Nu y t (substProc th p)
substProc th (Repl p) = Repl (substProc th p)   -- is this right?
substProc _  (Call p) = Call p  -- perhaps this shouldn't be substituted?

evalExpr :: MonadError ErrMsg m => Expr -> m Val
evalExpr (EV v) = return v
evalExpr (EAdd e1 e2) =
  VI <$> (liftM2 (+) (evalExpr e1 >>= unVI)
                     (evalExpr e2 >>= unVI))
evalExpr (ESub e1 e2) =
  VI <$> (liftM2 (-) (evalExpr e1 >>= unVI)
                     (evalExpr e2 >>= unVI))
evalExpr (EMul e1 e2) =
  VI <$> (liftM2 (*) (evalExpr e1 >>= unVI)
                     (evalExpr e2 >>= unVI))
evalExpr (EDiv e1 e2) =
  VI <$> (liftM2 (div) (evalExpr e1 >>= unVI)
                       (evalExpr e2 >>= unVI))
evalExpr (EEQ e1 e2) =
  VB <$> (liftM2 (==) (evalExpr e1 >>= unVB)
                      (evalExpr e2 >>= unVB))
evalExpr (ENEQ e1 e2) =
 VB <$> (liftM2 (/=) (evalExpr e1 >>= unVB)
                     (evalExpr e2 >>= unVB))
evalExpr (EGT e1 e2) =
  VB <$> (liftM2 (>) (evalExpr e1 >>= unVB)
                     (evalExpr e2 >>= unVB))
evalExpr (EGTE e1 e2) =
  VB <$> (liftM2 (>=) (evalExpr e1 >>= unVB)
                      (evalExpr e2 >>= unVB))
evalExpr (ELT e1 e2) =
  VB <$> (liftM2 (<) (evalExpr e1 >>= unVB)
                     (evalExpr e2 >>= unVB))
evalExpr (ELTE e1 e2) =
  VB <$> (liftM2 (<=) (evalExpr e1 >>= unVB)
                      (evalExpr e2 >>= unVB))
evalExpr (EIf e0 e1 e2) =
  (evalExpr e0 >>= unVB) >>= \v0 ->
  if v0 then evalExpr e1 else evalExpr e2
evalExpr (ETup es) = VT <$> mapM evalExpr es
evalExpr (EPrj i e) =
  (!! i) <$> (evalExpr e >>= unVT)

-- substitution related stuffs

match :: Ptrn -> Val -> Maybe Subst
match (PtrnVar x) v = Just $ Map.fromList [(PH x,v)]
match (PtrnLabel x) (VL y) | x == y = Just Map.empty
match (PtrnTuple xs) (VT vs) | length xs == length vs =
  Map.unions <$> mapM (uncurry match) (zip xs vs)
match _ _ = Nothing
--
-- joinSubs :: [Subst] -> Subst
-- joinSubs ss | nodup (map fst s) = s
--             | otherwise = error "non-linear pattern"
--   where s = Map.unions ss

matchClauses :: [Clause] -> Val -> Maybe (Subst, Proc)
matchClauses [] _ = Nothing
matchClauses ((Clause pt e):_) v
  | Just ss <- match pt v = Just (ss,e)
matchClauses (_:pps) v = matchClauses pps v

mask :: Ptrn -> Subst -> Subst
mask (PtrnVar x)      = Map.delete (PH x)
mask (PtrnTuple [])     = id
mask (PtrnTuple (p:ps)) = mask (PtrnTuple ps) . mask p
mask (PtrnLabel _)      = id

--------------------------------------------------------------------------------
-- | Free Vars

-- since it is used in type checking, we return only user defined names.
-- system generated names are supposed to exist only during execution.

freeVal :: Val -> Set Name
freeVal (VC (ND x)) = Set.singleton x
freeVal (VT vs) = Set.unions . map freeVal $ vs
freeVal _ = Set.empty

freeN :: Chan -> Set Name
freeN (ND x) = Set.singleton x
freeN _ = Set.empty

freeExpr :: Expr -> Set Name
freeExpr (EV v) = freeVal v
freeExpr (EAdd e1 e2) = freeExpr e1 `Set.union` freeExpr e2
freeExpr (ESub e1 e2) = freeExpr e1 `Set.union` freeExpr e2
freeExpr (EMul e1 e2) = freeExpr e1 `Set.union` freeExpr e2
freeExpr (EDiv e1 e2) = freeExpr e1 `Set.union` freeExpr e2
freeExpr (EEQ e1 e2) = freeExpr e1 `Set.union` freeExpr e2
freeExpr (ENEQ e1 e2) = freeExpr e1 `Set.union` freeExpr e2
freeExpr (EGT e1 e2) = freeExpr e1 `Set.union` freeExpr e2
freeExpr (EGTE e1 e2) = freeExpr e1 `Set.union` freeExpr e2
freeExpr (ELT e1 e2) = freeExpr e1 `Set.union` freeExpr e2
freeExpr (ELTE e1 e2) = freeExpr e1 `Set.union` freeExpr e2
freeExpr (EIf e0 e1 e2) = freeExpr e0 `Set.union` freeExpr e1 `Set.union` freeExpr e2
freeExpr (ETup es) = Set.unions (map freeExpr es)
freeExpr (EPrj _ e) = freeExpr e

freeProc :: Proc -> Set Name
freeProc End = Set.empty
freeProc (Send c e p) =
  freeN c `Set.union` freeExpr e `Set.union` freeProc p
freeProc (Recv c ps) =
  freeN c `Set.union` Set.unions (map freeClause ps)
freeProc (Par p1 p2) = freeProc p1 `Set.union` freeProc p2
freeProc (Nu x _ p) = freeProc p `Set.difference` Set.fromList [Pos x, Neg x]
freeProc (Repl p) = freeProc p -- is that right?
freeProc (Call _) = undefined -- what to do here?

freeClause :: Clause -> Set Name
freeClause (Clause ptn p) = Set.difference (freeProc p) (freePtrn ptn)

freePtrn :: Ptrn -> Set Name
freePtrn (PtrnVar x)  = Set.singleton (Pos x)
freePtrn (PtrnTuple xs) = Set.unions (map freePtrn xs)  -- linearity check?
freePtrn _       = Set.empty
