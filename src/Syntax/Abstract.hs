{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Syntax.Abstract where

import Control.Monad.Except
import Data.Text (Text, pack)

import qualified Syntax.Concrete as C
import Type
import Utilities

-- type Label = Text  -- moved to Type
type ErrMsg = String
type RName = Text   -- row name
type ProcName = Text

type SName = PN RName
data Name = ND (PN RName)   -- user defined
          | NG (PN Int)     -- system generated
          | NR ResName      -- reserved name
    deriving (Ord, Eq, Show)

data PName = PH RName        -- "pure" names, without polarization
           | PG Int
   deriving (Eq, Show)

data PN a = Pos a | Neg a -- | Neu a
    deriving (Ord, Eq, Show)

depolar :: PN a -> a
depolar (Pos x) = x
depolar (Neg x) = x
-- depolar (Neu x) = x

depolarCh :: Name -> PName
depolarCh (ND c) = PH (depolar c)
depolarCh (NG c) = PG (depolar c)
depolarCh (NR _) = error "bug: shouldn't call depolar without checking"

depolarCH :: Name -> RName
depolarCH = depolar . unND

unND :: Name -> PN RName
unND (ND n) = n
unND _ = undefined

data ResName = StdOut | StdIn
  deriving (Ord, Eq, Show)

data Prog = Prog [PiDecl]
  deriving (Eq, Show)
data PiDecl = PiDecl ProcName Pi
  deriving (Eq, Show)

data Pi = End
        | Send Name Expr Pi
        | Recv Name [Clause]
        | Par Pi Pi
        | Nu RName (Maybe Type) Pi
        | Repl Pi
        | Call ProcName
   deriving (Eq, Show)


data Clause = Clause Ptrn Pi
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

data Val = N Name
         | VI Int
         | VB Bool
         | VT [Val]    -- n-tuples
         | VL Label
         | VS Text
   deriving (Eq, Show)

data Ptrn = PN RName         -- patterns
          | PT [Ptrn]
          | PL Label
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

ePN :: String -> Expr
ePN = EV . N . ND . Pos . pack

eNN :: String -> Expr
eNN = EV . N . ND . Neg . pack

-- eN :: String -> Expr
-- eN  = EV . N . ND . Neu . pack

eI :: Int -> Expr
eI = EV . VI

eB :: Bool -> Expr
eB = EV . VB

eL :: String -> Expr
eL = EV . VL . pack

eR :: ResName -> Expr
eR = EV . N . NR

cP :: String -> Name
cP = ND . Pos . pack

cN :: String -> Name
cN = ND . Neg . pack

par :: Pi -> Pi -> Pi
End `par` p = p
p `par` End = p
p `par` q = Par p q

type Subst = FMap PName Val

substName :: Subst -> Name -> Name
substName _ (NR r) = NR r
substName th c =
  case lookup (depolarCh c) th of
    Just (N y) -> y
    Just _ -> error "not a name"
    Nothing -> c

substVal :: Subst -> Val -> Val
substVal _ (N (NR r)) = N (NR r)
substVal th (N c) | Just v <- lookup (depolarCh c) th = v
                  | otherwise                       = N c
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

substPi :: Subst -> Pi -> Pi
substPi _ End = End
substPi th (Send c u p) =
   Send (substName th c) (substExpr th u) (substPi th p)
substPi th (Recv c clauses) =
    if isEmpty th' then Recv c clauses
        else Recv (substName th' c)
                  (map (\(Clause ptrn p) -> Clause ptrn (substPi th' p)) clauses)
  where th' = foldr mask th (map (\(Clause ptrn _) -> ptrn) clauses)
substPi th (Par p q) = Par (substPi th p) (substPi th q)
substPi th (Nu y t p)
   | PH y `inDom` th = Nu y t p       -- is this right?
   | otherwise = Nu y t (substPi th p)
substPi th (Repl p) = Repl (substPi th p)   -- is this right?
substPi _  (Call p) = Call p  -- perhaps this shouldn't be substituted?

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
match (PN x) v = Just [(PH x,v)]
match (PL x) (VL y) | x == y = Just []
match (PT xs) (VT vs) | length xs == length vs =
  joinSubs <$> mapM (uncurry match) (zip xs vs)
match _ _ = Nothing

joinSubs :: [Subst] -> Subst
joinSubs ss | nodup (map fst s) = s
            | otherwise = error "non-linear pattern"
  where s = concat ss

matchClauses :: [Clause] -> Val -> Maybe (Subst, Pi)
matchClauses [] _ = Nothing
matchClauses ((Clause pt e):_) v
  | Just ss <- match pt v = Just (ss,e)
matchClauses (_:pps) v = matchClauses pps v

mask :: Ptrn -> Subst -> Subst
mask (PN x)      = rmEntry (PH x)
mask (PT [])     = id
mask (PT (p:ps)) = mask (PT ps) . mask p
mask (PL _)      = id

--------------------------------------------------------------------------------
-- | Free Vars

-- since it is used in type checking, we return only user defined names.
-- system generated names are supposed to exist only during execution.

freeVal :: Val -> [PN RName]
freeVal (N (ND x)) = [x]
freeVal (VT vs) = nubconcat . map freeVal $ vs
freeVal _ = []

freeN :: Name -> [PN RName]
freeN (ND x) = [x]
freeN _ = []

freeExpr :: Expr -> [PN RName]
freeExpr (EV v) = freeVal v
freeExpr (EAdd e1 e2) = freeExpr e1 `nubapp` freeExpr e2
freeExpr (ESub e1 e2) = freeExpr e1 `nubapp` freeExpr e2
freeExpr (EMul e1 e2) = freeExpr e1 `nubapp` freeExpr e2
freeExpr (EDiv e1 e2) = freeExpr e1 `nubapp` freeExpr e2
freeExpr (EEQ e1 e2) = freeExpr e1 `nubapp` freeExpr e2
freeExpr (ENEQ e1 e2) = freeExpr e1 `nubapp` freeExpr e2
freeExpr (EGT e1 e2) = freeExpr e1 `nubapp` freeExpr e2
freeExpr (EGTE e1 e2) = freeExpr e1 `nubapp` freeExpr e2
freeExpr (ELT e1 e2) = freeExpr e1 `nubapp` freeExpr e2
freeExpr (ELTE e1 e2) = freeExpr e1 `nubapp` freeExpr e2
freeExpr (EIf e0 e1 e2) = freeExpr e0 `nubapp` freeExpr e1 `nubapp` freeExpr e2
freeExpr (ETup es) = nubconcat (map freeExpr es)
freeExpr (EPrj _ e) = freeExpr e

freePi :: Pi -> [PN RName]
freePi End = []
freePi (Send c e p) =
  freeN c `nubapp` freeExpr e `nubapp` freePi p
freePi (Recv c ps) =
  freeN c `nubapp` nubconcat (map freeClause ps)
freePi (Par p1 p2) = freePi p1 `nubapp` freePi p2
freePi (Nu x _ p) = freePi p `setminus` [Pos x, Neg x]
freePi (Repl p) = freePi p -- is that right?
freePi (Call _) = undefined -- what to do here?

freeClause :: Clause -> [PN RName]
freeClause (Clause ptn p) = freePi p `setminus` freePtrn ptn

freePtrn :: Ptrn -> [PN RName]
freePtrn (PN x)  = [Pos x]
freePtrn (PT xs) = concat (map freePtrn xs)  -- linearity check?
freePtrn _       = []

--------------------------------------------------------------------------------
-- | Converting from Concrete Syntax Tree

class FromConcrete a b | a -> b where
  fromConcrete :: a -> b

instance FromConcrete (C.Program ann) Prog where
  fromConcrete (C.Program  declarations _) = Prog (map fromConcrete declarations)

instance FromConcrete (C.ProcDecl ann) PiDecl where
  fromConcrete (C.ProcDecl name process _) = PiDecl (fromConcrete name) (fromConcrete process)

instance FromConcrete (C.Label ann) Label where
  fromConcrete (C.Label    label _)     = label

instance FromConcrete (C.SimpName ann) RName where
  fromConcrete (C.SimpName name    _) = name

instance FromConcrete (C.Name ann) Name where
  fromConcrete (C.Positive name     _) = ND (Pos name)
  fromConcrete (C.Negative name     _) = ND (Neg name)
  fromConcrete (C.Reserved "stdin"  _) = NR StdIn
  fromConcrete (C.Reserved "stdout" _) = NR StdOut
  fromConcrete (C.Reserved _        _) = NR StdOut

instance FromConcrete (C.Pattern ann) Ptrn where
  fromConcrete (C.PtrnName name _)   = PN (fromConcrete name)
  fromConcrete (C.PtrnTuple patterns _) = PT (map fromConcrete patterns)
  fromConcrete (C.PtrnLabel label _) = PL (fromConcrete label)

instance FromConcrete (C.Clause ann) Clause where
  fromConcrete (C.Clause pattern process _) =
    Clause (fromConcrete pattern) (fromConcrete process)

instance FromConcrete (C.Process ann) Pi where
  fromConcrete (C.Nu name Nothing process _) =
    Nu (fromConcrete name) Nothing (fromConcrete process)
  fromConcrete (C.Nu name (Just t) process _) =
    Nu (fromConcrete name) (Just (fromConcrete t)) (fromConcrete process)
  fromConcrete (C.Send name expr process _) =
    Send (fromConcrete name) (fromConcrete expr) (fromConcrete process)
  fromConcrete (C.Recv name clauses _) =
    Recv (fromConcrete name) (map fromConcrete clauses)
  fromConcrete (C.Par procA procB _) =
    Par (fromConcrete procA) (fromConcrete procB)
  fromConcrete (C.Repl process _) =
    Repl (fromConcrete process)
  fromConcrete (C.Call name _) =
    Call (fromConcrete name)
  fromConcrete (C.End _) =
    End

instance FromConcrete (C.Expr ann) Expr where
  fromConcrete (C.Add x y _) = EAdd (fromConcrete x) (fromConcrete y)
  fromConcrete (C.Sub x y _) = ESub (fromConcrete x) (fromConcrete y)
  fromConcrete (C.Mul x y _) = EMul (fromConcrete x) (fromConcrete y)
  fromConcrete (C.Div x y _) = EDiv (fromConcrete x) (fromConcrete y)
  fromConcrete (C.EQ  x y _) = EEQ  (fromConcrete x) (fromConcrete y)
  fromConcrete (C.NEQ x y _) = ENEQ (fromConcrete x) (fromConcrete y)
  fromConcrete (C.GT  x y _) = EGT  (fromConcrete x) (fromConcrete y)
  fromConcrete (C.GTE x y _) = EGTE (fromConcrete x) (fromConcrete y)
  fromConcrete (C.LT  x y _) = ELT  (fromConcrete x) (fromConcrete y)
  fromConcrete (C.LTE x y _) = ELTE (fromConcrete x) (fromConcrete y)
  fromConcrete (C.IfThenElse p x y _) = EIf (fromConcrete p) (fromConcrete x) (fromConcrete y)
  fromConcrete (C.ExprBool b _) = EV (VB b)
  fromConcrete (C.ExprTuple xs _) = ETup (map fromConcrete xs)
  fromConcrete (C.ExprDigit x _) = EV (VI x)
  fromConcrete (C.ExprName  x _) = EV (N (fromConcrete x))
  fromConcrete (C.ExprLabel x _) = EV (VL (fromConcrete x))
  fromConcrete (C.ExprString x _) = EV (VS x)

instance FromConcrete (C.Sort ann) BType where
  fromConcrete (C.SortInt _)  = TInt
  fromConcrete (C.SortBool _) = TBool

instance FromConcrete (C.Type ann) Type where
  fromConcrete = undefined
{- To banacorn: please fixe this later. Thank you!
  fromConcrete (C.TypeEnd _             ) = TEnd
  fromConcrete (C.TypeSend (Left  s) t _) = TSend (Left (fromConcrete s)) (fromConcrete t)
  fromConcrete (C.TypeSend (Right s) t _) = TSend (Right (fromConcrete s)) (fromConcrete t)
  fromConcrete (C.TypeRecv (Left  s) t _) = TRecv (Left (fromConcrete s)) (fromConcrete t)
  fromConcrete (C.TypeRecv (Right s) t _) = TRecv (Right (fromConcrete s)) (fromConcrete t)
  fromConcrete (C.TypeSele selections  _) =
    TSele (map (\(C.TypeOfLabel l t _) -> (fromConcrete l, fromConcrete t)) selections)
  fromConcrete (C.TypeChoi choices     _) =
    TChoi (map (\(C.TypeOfLabel l t _) -> (fromConcrete l, fromConcrete t)) choices)
--  fromConcrete (C.TypeCall call        _) = TCall (fromConcrete call)
-}
