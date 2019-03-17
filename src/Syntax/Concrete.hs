{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Syntax.Concrete where

import qualified Syntax.Abstract as A
import qualified Type as A
import Data.Text (Text)
import Data.Loc
import Prelude hiding (LT, EQ, GT)

--------------------------------------------------------------------------------
-- | Concrete Syntax Tree

data Label    ann = Label     Text                                      ann
                  deriving (Show, Functor)
data Name     ann = Positive  Text                                      ann
                  | Negative  Text                                      ann
                  | Reserved  Text                                      ann
                  deriving (Show, Functor)

data Program  ann = Program   [Definition ann]                            ann
                  deriving (Show, Functor)

data ProcName ann = ProcName  Text                                      ann
                  deriving (Show, Functor)
data TypeName ann = TypeName  Int                                           ann
                  deriving (Show, Functor)

data Definition ann = ProcDefn  (ProcName ann)  (Process ann)             ann
                    | ChanType  (Name     ann)  (Type ann)             ann
                  deriving (Show, Functor)

data Process  ann = Send      (Name ann)     (Expr ann)         (Process ann) ann
                  | Recv      (Name ann)     [Clause ann]                     ann
                  | Nu        (ProcName ann) (Maybe (Type ann)) (Process ann) ann
                  | Par       (Process ann)  (Process ann)                    ann
                  | Call      (ProcName ann)                                  ann
                  | Repl      (Process ann)                                   ann
                  | End                                                       ann
                  deriving (Show, Functor)

data Pattern  ann = PtrnName  (ProcName ann)                            ann
                  | PtrnTuple [Pattern ann]                             ann
                  | PtrnLabel (Label ann)                               ann
                  deriving (Show, Functor)
data Clause   ann = Clause    (Pattern ann) (Process ann)               ann
                  deriving (Show, Functor)

-- Expressions and all that
data Expr     ann = ExprTuple [Expr ann]                                ann
                  | Mul       (Expr ann) (Expr ann)                     ann
                  | Div       (Expr ann) (Expr ann)                     ann
                  | Add       (Expr ann) (Expr ann)                     ann
                  | Sub       (Expr ann) (Expr ann)                     ann
                  | EQ        (Expr ann) (Expr ann)                     ann
                  | NEQ       (Expr ann) (Expr ann)                     ann
                  | GT        (Expr ann) (Expr ann)                     ann
                  | GTE       (Expr ann) (Expr ann)                     ann
                  | LT        (Expr ann) (Expr ann)                     ann
                  | LTE       (Expr ann) (Expr ann)                     ann
                  | IfThenElse (Expr ann) (Expr ann) (Expr ann)         ann
                  | ExprBool  Bool                                      ann
                  | ExprDigit Int                                       ann
                  | ExprName  (Name ann)                                ann
                  | ExprLabel (Label ann)                               ann
                  | ExprString Text                                     ann
                  deriving (Show, Functor)

-- Session Types
data BaseType ann
              = BaseInt                                                 ann
              | BaseBool                                                ann
              deriving (Show, Functor)
data Type ann = TypeEnd                                                 ann
              | TypeBase      (BaseType ann)                            ann
              | TypeTuple     [Type ann]                                ann
              | TypeSend      (Type ann) (Type ann)                     ann
              | TypeRecv      (Type ann) (Type ann)                     ann
              | TypeSele      [TypeOfLabel ann]                         ann
              | TypeChoi      [TypeOfLabel ann]                         ann
              | TypeUn        (Type ann)                                ann
              | TypeVar       (TypeName ann)                            ann
              | TypeMu        (Type ann)                                ann
              deriving (Show)
data TypeOfLabel ann = TypeOfLabel (Label ann) (Type ann)               ann
              deriving (Show, Functor)

instance Functor Type where
  fmap f (TypeEnd ann)              = TypeEnd                                 (f ann)
  fmap f (TypeTuple a ann)          = TypeTuple (map (fmap f) a)                    (f ann)
  fmap f (TypeBase a ann)           = TypeBase  (fmap f a)                    (f ann)
  fmap f (TypeSend a b ann)         = TypeSend  (fmap f a) (fmap f b) (f ann)
  fmap f (TypeRecv a b ann)         = TypeRecv  (fmap f a) (fmap f b) (f ann)
  fmap f (TypeSele a ann)           = TypeSele  (map (fmap f) a)              (f ann)
  fmap f (TypeChoi a ann)           = TypeChoi  (map (fmap f) a)              (f ann)
  fmap f (TypeUn a ann)             = TypeUn    (fmap f a)                    (f ann)
  fmap f (TypeVar a ann)            = TypeVar  (fmap f a)                    (f ann)
  fmap f (TypeMu a ann)             = TypeMu    (fmap f a)                    (f ann)

--------------------------------------------------------------------------------
-- | Instance of Located

instance Located (Name Loc) where
  locOf (Positive _ loc) = loc
  locOf (Negative _ loc) = loc
  locOf (Reserved _ loc) = loc

instance Located (ProcName Loc) where
  locOf (ProcName _ loc) = loc

instance Located (Label Loc) where
  locOf (Label _ loc) = loc

instance Located (Program Loc) where
  locOf (Program _ loc) = loc

instance Located (Definition Loc) where
  locOf (ProcDefn _ _ loc) = loc

instance Located (Process Loc) where
  locOf (Send _ _ _ loc) = loc
  locOf (Recv _ _ loc) = loc
  locOf (Nu _ _ _ loc) = loc
  locOf (Par _ _ loc) = loc
  locOf (Call _ loc) = loc
  locOf (Repl _ loc) = loc
  locOf (End loc) = loc

instance Located (Pattern Loc) where
  locOf (PtrnName _ loc) = loc
  locOf (PtrnTuple _ loc) = loc
  locOf (PtrnLabel _ loc) = loc

instance Located (Clause Loc) where
  locOf (Clause _ _ loc) = loc

instance Located (Expr Loc) where
  locOf (ExprTuple _ loc) = loc
  locOf (Mul _ _ loc) = loc
  locOf (Div _ _ loc) = loc
  locOf (Add _ _ loc) = loc
  locOf (Sub _ _ loc) = loc
  locOf (EQ _ _ loc) = loc
  locOf (NEQ _ _ loc) = loc
  locOf (GT _ _ loc) = loc
  locOf (GTE _ _ loc) = loc
  locOf (LT _ _ loc) = loc
  locOf (LTE _ _ loc) = loc
  locOf (IfThenElse _ _ _ loc) = loc
  locOf (ExprBool _ loc) = loc
  locOf (ExprDigit _ loc) = loc
  locOf (ExprName _ loc) = loc
  locOf (ExprLabel _ loc) = loc
  locOf (ExprString _ loc) = loc

instance Located (BaseType Loc) where
  locOf (BaseInt loc) = loc
  locOf (BaseBool loc) = loc

instance Located (Type Loc) where
  locOf (TypeEnd loc) = loc
  locOf (TypeBase _ loc) = loc
  locOf (TypeTuple _ loc) = loc
  locOf (TypeSend _ _ loc) = loc
  locOf (TypeRecv _ _ loc) = loc
  locOf (TypeSele _ loc) = loc
  locOf (TypeChoi _ loc) = loc
  locOf (TypeUn _ loc) = loc
  locOf (TypeVar _ loc) = loc
  locOf (TypeMu _ loc) = loc

instance Located (TypeOfLabel Loc) where
  locOf (TypeOfLabel _ _ loc) = loc

--------------------------------------------------------------------------------
-- | Converting to Abstract Syntax Tree

class ToAbstract a b | a -> b where
  toAbstract :: a -> b

instance ToAbstract (Program ann) A.Program where
  toAbstract (Program  definitions _) = A.Program (map toAbstract definitions)

instance ToAbstract (Definition ann) A.Definition where
  toAbstract (ProcDefn name process _) = A.ProcDefn (toAbstract name) (toAbstract process)
  toAbstract (ChanType name t _) = A.ChanType (toAbstract name) (toAbstract t)

instance ToAbstract (Label ann) A.Label where
  toAbstract (Label    label _)     = label

instance ToAbstract (ProcName ann) Text where
  toAbstract (ProcName name    _) = name

instance ToAbstract (Name ann) A.Name where
  toAbstract (Positive name     _) = A.ND (A.Pos name)
  toAbstract (Negative name     _) = A.ND (A.Neg name)
  toAbstract (Reserved "stdin"  _) = A.NR A.StdIn
  toAbstract (Reserved "stdout" _) = A.NR A.StdOut
  toAbstract (Reserved _        _) = A.NR A.StdOut

instance ToAbstract (Pattern ann) A.Ptrn where
  toAbstract (PtrnName name _)   = A.PN (toAbstract name)
  toAbstract (PtrnTuple patterns _) = A.PT (map toAbstract patterns)
  toAbstract (PtrnLabel label _) = A.PL (toAbstract label)

instance ToAbstract (Clause ann) A.Clause where
  toAbstract (Clause pattern process _) =
    A.Clause (toAbstract pattern) (toAbstract process)

instance ToAbstract (Process ann) A.Pi where
  toAbstract (Nu name Nothing process _) =
    A.Nu (toAbstract name) Nothing (toAbstract process)
  toAbstract (Nu name (Just t) process _) =
    A.Nu (toAbstract name) (Just (toAbstract t)) (toAbstract process)
  toAbstract (Send name expr process _) =
    A.Send (toAbstract name) (toAbstract expr) (toAbstract process)
  toAbstract (Recv name clauses _) =
    A.Recv (toAbstract name) (map toAbstract clauses)
  toAbstract (Par procA procB _) =
    A.Par (toAbstract procA) (toAbstract procB)
  toAbstract (Repl process _) =
    A.Repl (toAbstract process)
  toAbstract (Call name _) =
    A.Call (toAbstract name)
  toAbstract (End _) =
    A.End

instance ToAbstract (Expr ann) A.Expr where
  toAbstract (Add x y _) = A.EAdd (toAbstract x) (toAbstract y)
  toAbstract (Sub x y _) = A.ESub (toAbstract x) (toAbstract y)
  toAbstract (Mul x y _) = A.EMul (toAbstract x) (toAbstract y)
  toAbstract (Div x y _) = A.EDiv (toAbstract x) (toAbstract y)
  toAbstract (EQ  x y _) = A.EEQ  (toAbstract x) (toAbstract y)
  toAbstract (NEQ x y _) = A.ENEQ (toAbstract x) (toAbstract y)
  toAbstract (GT  x y _) = A.EGT  (toAbstract x) (toAbstract y)
  toAbstract (GTE x y _) = A.EGTE (toAbstract x) (toAbstract y)
  toAbstract (LT  x y _) = A.ELT  (toAbstract x) (toAbstract y)
  toAbstract (LTE x y _) = A.ELTE (toAbstract x) (toAbstract y)
  toAbstract (IfThenElse p x y _) = A.EIf (toAbstract p) (toAbstract x) (toAbstract y)
  toAbstract (ExprBool b _) = A.EV (A.VB b)
  toAbstract (ExprTuple xs _) = A.ETup (map toAbstract xs)
  toAbstract (ExprDigit x _) = A.EV (A.VI x)
  toAbstract (ExprName  x _) = A.EV (A.N (toAbstract x))
  toAbstract (ExprLabel x _) = A.EV (A.VL (toAbstract x))
  toAbstract (ExprString x _) = A.EV (A.VS x)

instance ToAbstract (TypeName ann) Int where
  toAbstract (TypeName name    _) = name

instance ToAbstract (BaseType ann) A.BType where
  toAbstract (BaseInt _)  = A.TInt
  toAbstract (BaseBool _) = A.TBool

instance ToAbstract (Type ann) A.Type where
  toAbstract (TypeEnd _) = A.TEnd
  toAbstract (TypeBase t _) = A.TBase (toAbstract t)
  toAbstract (TypeTuple ts _) = A.TTuple (map toAbstract ts)
  toAbstract (TypeSend t u _) = A.TSend (toAbstract t) (toAbstract u)
  toAbstract (TypeRecv t u _) = A.TRecv (toAbstract t) (toAbstract u)
  toAbstract (TypeSele ts _) = A.TSele (map toAbstract ts)
  toAbstract (TypeChoi ts _) = A.TChoi (map toAbstract ts)
  toAbstract (TypeUn t _) = A.TUn (toAbstract t)
  toAbstract (TypeVar t _) = A.TVar (toAbstract t)
  toAbstract (TypeMu t _) = A.TMu (toAbstract t)

instance ToAbstract (TypeOfLabel ann) (A.Label, A.Type) where
  toAbstract (TypeOfLabel t u _)  = (toAbstract t, toAbstract u)

{- To banacorn: please fixe this later. Thank you!
  toAbstract (TypeEnd _             ) = TEnd
  toAbstract (TypeSend (Left  s) t _) = TSend (Left (toAbstract s)) (toAbstract t)
  toAbstract (TypeSend (Right s) t _) = TSend (Right (toAbstract s)) (toAbstract t)
  toAbstract (TypeRecv (Left  s) t _) = TRecv (Left (toAbstract s)) (toAbstract t)
  toAbstract (TypeRecv (Right s) t _) = TRecv (Right (toAbstract s)) (toAbstract t)
  toAbstract (TypeSele selections  _) =
    TSele (map (\(TypeOfLabel l t _) -> (toAbstract l, toAbstract t)) selections)
  toAbstract (TypeChoi choices     _) =
    TChoi (map (\(TypeOfLabel l t _) -> (toAbstract l, toAbstract t)) choices)
--  toAbstract (TypeCall call        _) = TCall (toAbstract call)
-}
 -- TEnd                    -- end
 --           | TBase BType
 --           | TTuple [Type]
 --           | TSend Type Type       -- send
 --           | TRecv Type Type       -- recv
 --           | TSele [(Label, Type)]  -- select
 --           | TChoi [(Label, Type)]  -- choice
 --           | TUn Type               -- unrestricted
 --
 --           | TVar Int    -- de Bruin index?
 --           | TMu Type
 --  deriving (Eq, Show)
