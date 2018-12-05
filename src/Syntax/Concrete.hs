{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Syntax.Concrete where

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

data Program  ann = Program   [ProcDecl ann]                            ann
                  deriving (Show, Functor)

data SimpName ann = SimpName  Text                                      ann
                  deriving (Show, Functor)
data ProcDecl ann = ProcDecl  (SimpName ann)  (Process ann)             ann
                  deriving (Show, Functor)

data Process  ann = Send      (Name ann)     (Expr ann)         (Process ann) ann
                  | Recv      (Name ann)     [Clause ann]                     ann
                  | Nu        (SimpName ann) (Maybe (Type ann)) (Process ann) ann
                  | Par       (Process ann)  (Process ann)                    ann
                  | Call      (SimpName ann)                                  ann
                  | End                                                       ann
                  deriving (Show, Functor)

data Pattern  ann = PtrnName  (SimpName ann)                            ann
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
data Sort ann = SortInt                                                 ann
              | SortBool                                                ann
              -- | SortTuple     [Sort ann]                                ann
              deriving (Show, Functor)
data Type ann = TypeEnd                                                 ann
              | TypeSend      (Either (Sort ann) (Type ann)) (Type ann) ann
              | TypeRecv      (Either (Sort ann) (Type ann)) (Type ann) ann
              | TypeSele      [TypeOfLabel ann]                         ann
              | TypeChoi      [TypeOfLabel ann]                         ann
              | TypeCall      (SimpName ann)                            ann
              deriving (Show)
data TypeOfLabel ann = TypeOfLabel (Label ann) (Type ann)               ann
              deriving (Show, Functor)

instance Functor Type where
  fmap f (TypeEnd ann)              = TypeEnd                                (f ann)
  fmap f (TypeSend (Left a) b ann)  = TypeSend (Left  $ fmap f a) (fmap f b) (f ann)
  fmap f (TypeSend (Right a) b ann) = TypeSend (Right $ fmap f a) (fmap f b) (f ann)
  fmap f (TypeRecv (Left a) b ann)  = TypeRecv (Left  $ fmap f a) (fmap f b) (f ann)
  fmap f (TypeRecv (Right a) b ann) = TypeRecv (Right $ fmap f a) (fmap f b) (f ann)
  fmap f (TypeSele a ann)           = TypeSele (map (fmap f) a)              (f ann)
  fmap f (TypeChoi a ann)           = TypeChoi (map (fmap f) a)              (f ann)
  fmap f (TypeCall a ann)           = TypeCall (fmap f a)                    (f ann)

--------------------------------------------------------------------------------
-- | Instance of Located

instance Located (Name Loc) where
  locOf (Positive _ loc) = loc
  locOf (Negative _ loc) = loc
  locOf (Reserved _ loc) = loc

instance Located (SimpName Loc) where
  locOf (SimpName _ loc) = loc

instance Located (Label Loc) where
  locOf (Label _ loc) = loc

instance Located (Program Loc) where
  locOf (Program _ loc) = loc

instance Located (ProcDecl Loc) where
  locOf (ProcDecl _ _ loc) = loc

instance Located (Process Loc) where
  locOf (Send _ _ _ loc) = loc
  locOf (Recv _ _ loc) = loc
  locOf (Nu _ _ _ loc) = loc
  locOf (Par _ _ loc) = loc
  locOf (Call _ loc) = loc
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

instance Located (Sort Loc) where
  locOf (SortInt loc) = loc
  locOf (SortBool loc) = loc

instance Located (Type Loc) where
  locOf (TypeSend _ _ loc) = loc
  locOf (TypeRecv _ _ loc) = loc
  locOf (TypeSele _ loc) = loc
  locOf (TypeChoi _ loc) = loc
  locOf (TypeCall _ loc) = loc
  locOf (TypeEnd loc) = loc

instance Located (TypeOfLabel Loc) where
  locOf (TypeOfLabel _ _ loc) = loc

  -- bimap :: (Pos -> Pos) -> (Pos -> Pos) -> Loc -> Loc
-- bimap _ _ NoLoc     = NoLoc
-- bimap f g (Loc a b) = Loc (f a) (g b)
--
-- class RestoreLocation a where
--   restore :: a Loc -> a Loc
--
-- instance RestoreLocation Program where
--   restore (Program decl NoLoc) = Program (map restore decl) $ NoLoc
--   restore (Program decl loc)   = Program (map restore decl) $ bimap (startPos . posFile) id loc
--
-- instance RestoreLocation ProcDecl where
--   restore (ProcDecl name process NoLoc) = ProcDecl name process $ NoLoc
--   restore (ProcDecl name process loc)   = ProcDecl name' process $ bimap (const $ startPos name') id loc
--     where name' = restore name
