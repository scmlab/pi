{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Syntax.Abstract where

import Control.Monad.Except
import Data.Text (Text)

import qualified Syntax.Concrete as C
import Utilities

type Label = Text
type ErrMsg = String
type RName = Text   -- row name

data Name = NS RName    -- user defined
          | NG Int      -- system generated
          | NR ResName  -- reserved name
    deriving (Eq, Show)

data ResName = StdOut | StdIn
  deriving (Eq, Show)

data Prog = Prog [PiDecl]
  deriving (Eq, Show)
data PiDecl = PiDecl Name Pi
  deriving (Eq, Show)

data Pi = End
        | Send Name Expr Pi
        | Recv Name [Clause]
        | Par Pi Pi
        | Nu Name Pi
        | Call Name
   deriving (Eq, Show)

data Clause = Clause Ptrn Pi
   deriving (Eq, Show)

data Expr = EV Val
          | EPlus Expr Expr
          | EMinus Expr Expr

          | EIf Expr Expr Expr

          | ETup [Expr]     -- n-tuples
          | EPrj Int Expr   -- projection of tuples
   deriving (Eq, Show)

data Val = N Name
         | VI Int
         | VB Bool
         | VT [Val]    -- n-tuples
         | VL Label
   deriving (Eq, Show)

data Ptrn = PN Name         -- patterns
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

eN :: Name -> Expr
eN = EV . N

eI :: Int -> Expr
eI = EV . VI

eL :: Label -> Expr
eL = EV . VL

eR :: ResName -> Expr
eR = EV . N . NR

par :: Pi -> Pi -> Pi
End `par` p = p
p `par` End = p
p `par` q = Par p q

type Subst = FMap Name Val

substName :: Subst -> Name -> Name
substName th x =
  case lookup x th of
    Just (N y) -> y
    Just _ -> error "not a name"
    Nothing -> x

substVal :: Subst -> Val -> Val
substVal th (N y) | Just v <- lookup y th = v
                  | otherwise             = N y
substVal th (VT vs) = VT (map (substVal th) vs)
substVal _ u = u

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
substPi _ End = End
substPi th (Send c u p) =
   Send (substName th c) (substExpr th u) (substPi th p)
substPi th (Recv c clauses) =
    if isEmpty th' then Recv c clauses
        else Recv (substName th' c)
                  (map (\(Clause ptrn p) -> Clause ptrn (substPi th' p)) clauses)
  where th' = foldr mask th (map (\(Clause ptrn _) -> ptrn) clauses)
substPi th (Par p q) = Par (substPi th p) (substPi th q)
substPi th (Nu y p)
   | y `inDom` th = Nu y p       -- is this right?
   | otherwise = Nu y (substPi th p)
substPi _ (Call p) = Call p  -- perhaps this shouldn't be substituted?

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

matchClauses :: [Clause] -> Val -> Maybe (Subst, Pi)
matchClauses [] _ = Nothing
matchClauses ((Clause pt e):_) v
  | Just ss <- match pt v = Just (ss,e)
matchClauses (_:pps) v = matchClauses pps v

mask :: Ptrn -> Subst -> Subst
mask (PN x)      = rmEntry x
mask (PT [])     = id
mask (PT (p:ps)) = mask (PT ps) . mask p
mask (PL _)      = id

--------------------------------------------------------------------------------
-- | Converting from Concrete Syntax Tree

class FromConcrete a b | a -> b where
  fromConcrete :: a -> b

instance FromConcrete C.Program Prog where
  fromConcrete (C.Program _ declarations) = Prog (map fromConcrete declarations)

instance FromConcrete C.ProcDecl PiDecl where
  fromConcrete (C.ProcDecl _ name process) = PiDecl (fromConcrete name) (fromConcrete process)

instance FromConcrete C.Label Label where
  fromConcrete (C.Label    _ label)     = label

instance FromConcrete C.Name Name where
  fromConcrete (C.Name     _ name)      = NS name
  fromConcrete (C.Reserved _ "stdin")   = NR StdOut
  fromConcrete (C.Reserved _ "stdout")  = NR StdOut
  fromConcrete (C.Reserved _ name)      = NS name

instance FromConcrete C.Pattern Ptrn where
  fromConcrete (C.PtrnName name) = PN (fromConcrete name)
  fromConcrete (C.PtrnLabel label) = PL (fromConcrete label)

instance FromConcrete C.Clause Clause where
  fromConcrete (C.Clause _ pattern process) =
    Clause (fromConcrete pattern) (fromConcrete process)

instance FromConcrete C.Process Pi where
  fromConcrete (C.Nu _ name process) =
    Nu (fromConcrete name) (fromConcrete process)
  fromConcrete (C.Send _ name expr process) =
    Send (fromConcrete name) (fromConcrete expr) (fromConcrete process)
  fromConcrete (C.Recv _ name clauses) =
    Recv (fromConcrete name) (map fromConcrete clauses)
  fromConcrete (C.Par _ procA procB) =
    Par (fromConcrete procA) (fromConcrete procB)
  fromConcrete (C.Call _ name) =
    Call (fromConcrete name)
  fromConcrete (C.End _) =
    End

instance FromConcrete C.Expr Expr where
  -- WARNING: there's no multiplication or division here in the AST!
  fromConcrete (C.Mul _ x _) = fromConcrete x
  fromConcrete (C.Div _ x _) = fromConcrete x
  fromConcrete (C.Add _ x y) = EPlus (fromConcrete x) (fromConcrete y)
  fromConcrete (C.Sub _ x y) = EMinus (fromConcrete x) (fromConcrete y)
  fromConcrete (C.ExprDigit _ x) = EV (VI x)
  fromConcrete (C.ExprName  x) = EV (N (fromConcrete x))
  fromConcrete (C.ExprLabel x) = EV (VL (fromConcrete x))
