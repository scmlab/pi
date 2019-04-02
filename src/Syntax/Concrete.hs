{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Syntax.Concrete where

import qualified Syntax.Abstract as A
import qualified Type as A
import Data.Text (Text)
import Data.Loc (Loc, Located(..))
import Prelude hiding (LT, EQ, GT)

--------------------------------------------------------------------------------
-- | Concrete Syntax Tree

data Label        = Label     Text  Loc
                  deriving (Show)
data Chan         = Pos  Text  Loc
                  | Neg  Text  Loc
                  | Res  Text  Loc
                  deriving (Show)

data Program      = Program   [Definition]  Loc
                  deriving (Show)
data ProcName     = ProcName  Text  Loc
                  deriving (Show)
data TypeName     = TypeName  Text  Loc
                  deriving (Show)

data Definition   = ProcDefn  ProcName  Proc Loc
                  | ChanType  Chan      Type Loc
                  | TypeDefn  TypeName  Type Loc
                  deriving (Show)

data Proc         = Send      Chan      Expr          Proc  Loc
                  | Recv      Chan      [Clause]            Loc
                  | Nu        ProcName  (Maybe Type)  Proc  Loc
                  | Par       Proc                    Proc  Loc
                  | Call      ProcName                      Loc
                  | Repl      Proc                          Loc
                  | End                                     Loc
                  deriving (Show)

data Ptrn         = PtrnName  ProcName                      Loc
                  | PtrnTuple [Ptrn]                        Loc
                  | PtrnLabel Label                         Loc
                  deriving (Show)
data Clause       = Clause    Ptrn Proc                     Loc
                  deriving (Show)

-- Expressions and all that
data Expr         = ExprTuple [Expr]    Loc
                  | Mul       Expr Expr Loc
                  | Div       Expr Expr Loc
                  | Add       Expr Expr Loc
                  | Sub       Expr Expr Loc
                  | EQ        Expr Expr Loc
                  | NEQ       Expr Expr Loc
                  | GT        Expr Expr Loc
                  | GTE       Expr Expr Loc
                  | LT        Expr Expr Loc
                  | LTE       Expr Expr Loc
                  | IfThenElse Expr Expr Expr Loc
                  | ExprBool  Bool Loc
                  | ExprDigit Int Loc
                  | ExprChan  Chan Loc
                  | ExprLabel Label Loc
                  | ExprString Text Loc
                  deriving (Show)

-- Session Types

data TypeVar  = TypeVarIndex Int Loc
              | TypeVarText TypeName Loc
                  deriving (Show)

data BaseType = BaseInt Loc
              | BaseBool Loc
              deriving (Show)
data Type     = TypeEnd                         Loc
              | TypeBase      BaseType          Loc
              | TypeTuple     [Type]            Loc
              | TypeSend      Type Type         Loc
              | TypeRecv      Type Type         Loc
              | TypeSele      [TypeOfLabel]     Loc
              | TypeChoi      [TypeOfLabel]     Loc
              | TypeUn        Type              Loc
              | TypeVar       TypeVar           Loc
              | TypeMu        Type              Loc
              deriving (Show)
data TypeOfLabel = TypeOfLabel Label Type   Loc
              deriving (Show)

--------------------------------------------------------------------------------
-- | Instance of Located

instance Located Chan where
  locOf (Pos _ loc) = loc
  locOf (Neg _ loc) = loc
  locOf (Res _ loc) = loc

instance Located ProcName where
  locOf (ProcName _ loc) = loc

instance Located Label where
  locOf (Label _ loc) = loc

instance Located Program where
  locOf (Program _ loc) = loc

instance Located Definition where
  locOf (ProcDefn _ _ loc) = loc
  locOf (TypeDefn _ _ loc) = loc
  locOf (ChanType _ _ loc) = loc

instance Located Proc where
  locOf (Send _ _ _ loc) = loc
  locOf (Recv _ _ loc) = loc
  locOf (Nu _ _ _ loc) = loc
  locOf (Par _ _ loc) = loc
  locOf (Call _ loc) = loc
  locOf (Repl _ loc) = loc
  locOf (End loc) = loc

instance Located Ptrn where
  locOf (PtrnName _ loc) = loc
  locOf (PtrnTuple _ loc) = loc
  locOf (PtrnLabel _ loc) = loc

instance Located Clause where
  locOf (Clause _ _ loc) = loc

instance Located Expr where
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
  locOf (ExprChan _ loc) = loc
  locOf (ExprLabel _ loc) = loc
  locOf (ExprString _ loc) = loc

instance Located BaseType where
  locOf (BaseInt loc) = loc
  locOf (BaseBool loc) = loc

instance Located Type where
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

instance Located TypeOfLabel where
  locOf (TypeOfLabel _ _ loc) = loc

--------------------------------------------------------------------------------
-- | Converting to Abstract Syntax Tree

class ToAbstract a b | a -> b where
  toAbstract :: a -> b

instance ToAbstract Program A.Program where
  toAbstract (Program  definitions _) = A.Program (map toAbstract definitions)

instance ToAbstract Definition A.Definition where
  toAbstract (ProcDefn name process _) = A.ProcDefn (toAbstract name) (toAbstract process)
  toAbstract (ChanType name t _) = A.ChanType (toAbstract name) (toAbstract t)
  toAbstract (TypeDefn name t _) = A.TypeDefn (toAbstract name) (toAbstract t)

instance ToAbstract Label A.Label where
  toAbstract (Label    label _)     = label

instance ToAbstract ProcName Text where
  toAbstract (ProcName name    _) = name

instance ToAbstract Chan A.Chan where
  toAbstract (Pos name     _) = A.ND (A.Pos name)
  toAbstract (Neg name     _) = A.ND (A.Neg name)
  toAbstract (Res "stdin"  _) = A.NR A.StdIn
  toAbstract (Res "stdout" _) = A.NR A.StdOut
  toAbstract (Res _        _) = A.NR A.StdOut

instance ToAbstract Ptrn A.Ptrn where
  toAbstract (PtrnName name _)   = A.PtrnName (toAbstract name)
  toAbstract (PtrnTuple patterns _) = A.PtrnTuple (map toAbstract patterns)
  toAbstract (PtrnLabel label _) = A.PtrnLabel (toAbstract label)

instance ToAbstract Clause A.Clause where
  toAbstract (Clause pattern process _) =
    A.Clause (toAbstract pattern) (toAbstract process)

instance ToAbstract Proc A.Proc where
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

instance ToAbstract Expr A.Expr where
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
  toAbstract (ExprChan  x _) = A.EV (A.VC (toAbstract x))
  toAbstract (ExprLabel x _) = A.EV (A.VL (toAbstract x))
  toAbstract (ExprString x _) = A.EV (A.VS x)

instance ToAbstract TypeVar A.TypeVar where
  toAbstract (TypeVarIndex name _) = A.TypeVarIndex name
  toAbstract (TypeVarText  name _) = A.TypeVarText (toAbstract name)

instance ToAbstract TypeName A.TypeName where
  toAbstract (TypeName name    _) = name

instance ToAbstract BaseType A.BType where
  toAbstract (BaseInt _)  = A.TInt
  toAbstract (BaseBool _) = A.TBool

instance ToAbstract Type A.Type where
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

instance ToAbstract TypeOfLabel (A.Label, A.Type) where
  toAbstract (TypeOfLabel t u _)  = (toAbstract t, toAbstract u)
