{-# LANGUAGE DeriveFunctor                  #-}

module Syntax.Concrete where

import Data.Text (Text)

--------------------------------------------------------------------------------
-- | Concrete Syntax Tree

data Label    ann = Label     Text                                      ann
                  deriving (Show)
data Name     ann = Positive  Text                                      ann
                  | Negative  Text                                      ann
                  | Reserved  Text                                      ann
                  deriving (Show)

data Program  ann = Program   [ProcDecl ann]                            ann
                  deriving (Show)

data SimpName ann = SimpName  Text                                      ann
                  deriving (Show)
data ProcDecl ann = ProcDecl  (SimpName ann)  (Process ann)             ann
                  deriving (Show)

data Process  ann = Send      (Name ann)     (Expr ann)         (Process ann) ann
                  | Recv      (Name ann)     [Clause ann]                     ann
                  | Nu        (SimpName ann) (Maybe (Type ann)) (Process ann) ann
                  | Par       (Process ann)  (Process ann)                    ann
                  | Call      (SimpName ann)                                  ann
                  | End                                                       ann
                  deriving (Show)

data Pattern  ann = PtrnName  (SimpName ann)                            ann
                  | PtrnTuple [Pattern ann]                             ann
                  | PtrnLabel (Label ann)                               ann
                  deriving (Show)
data Clause   ann = Clause    (Pattern ann) (Process ann)               ann
                  deriving (Show)

-- Expressions and all that
data Expr     ann = ExprTuple [Expr ann]                                ann
                  | Mul       (Expr ann) (Expr ann)                     ann
                  | Div       (Expr ann) (Expr ann)                     ann
                  | Add       (Expr ann) (Expr ann)                     ann
                  | Sub       (Expr ann) (Expr ann)                     ann
                  | ExprDigit Int                                       ann
                  | ExprName  (Name ann)                                ann
                  | ExprLabel (Label ann)                               ann
                  deriving (Show)

-- Session Types
data Sort ann = SortInt                                                 ann
              | SortBool                                                ann
              -- | SortTuple     [Sort ann]                                ann
              deriving (Show)
data Type ann = TypeEnd                                                 ann
              | TypeSend      (Either (Sort ann) (Type ann)) (Type ann) ann
              | TypeRecv      (Either (Sort ann) (Type ann)) (Type ann) ann
              | TypeSele      [TypeOfLabel ann]                         ann
              | TypeChoi      [TypeOfLabel ann]                         ann
              | TypeCall      (SimpName ann)                            ann
              deriving (Show)
data TypeOfLabel ann = TypeOfLabel (Label ann) (Type ann)               ann
              deriving (Show)
