{-# LANGUAGE DeriveFunctor                  #-}

module Syntax.Concrete where

import Data.Text (Text)

--------------------------------------------------------------------------------
-- | Concrete Syntax Tree

data Label    ann = Label     Text                                      ann
                  deriving (Show, Functor)
data Name     ann = Name      Text                                      ann
                  | Reserved  Text                                      ann
                  deriving (Show, Functor)

data Program  ann = Program   [ProcDecl ann]                            ann
                  deriving (Show, Functor)
data ProcDecl ann = ProcDecl  (Name ann)    (Process ann)               ann
                  deriving (Show, Functor)

data Process  ann = Send      (Name ann)    (Expr ann)    (Process ann) ann
                  | Recv      (Name ann)    [Clause ann]                ann
                  | Nu        (Name ann)    (Process ann)               ann
                  | Par       (Process ann) (Process ann)               ann
                  | Call      (Name ann)                                ann
                  | End                                                 ann
                  deriving (Show, Functor)

data Pattern  ann = PtrnName  (Name ann)                                ann
                  | PtrnLabel (Label ann)                               ann
                  deriving (Show, Functor)
data Clause   ann = Clause    (Pattern ann) (Process ann)               ann
                  deriving (Show, Functor)

-- Expressions and all that
data Expr     ann = Mul       (Expr ann) (Expr ann)                     ann
                  | Div       (Expr ann) (Expr ann)                     ann
                  | Add       (Expr ann) (Expr ann)                     ann
                  | Sub       (Expr ann) (Expr ann)                     ann
                  | ExprDigit Int                                       ann
                  | ExprName  (Name ann)                                ann
                  | ExprLabel (Label ann)                               ann
                  deriving (Show, Functor)


-- data Label    = Label     Range Text          deriving (Show, Functor)
-- data Name     = Name      Range Text
--               | Reserved  Range Text          deriving (Show, Functor)
--
-- data Program  = Program   Range [ProcDecl]    deriving (Show, Functor)
-- data ProcDecl = ProcDecl  Range Name Process  deriving (Show, Functor)
--
-- data Process  = Send      Range Name Expr Process
--               | Recv      Range Name      [Clause]
--               | Nu        Range Name      Process
--               | Par       Range           Process Process
--               | Call      Range Name
--               | End       Range
--               deriving (Show, Functor)
--
-- data Pattern  = PtrnName  Name
--               | PtrnLabel Label
--               deriving (Show, Functor)
-- data Clause   = Clause    Range Pattern Process        deriving (Show, Functor)
--
-- -- Expressions and all that
-- data Expr     = Mul       Range Expr Expr
--               | Div       Range Expr Expr
--               | Add       Range Expr Expr
--               | Sub       Range Expr Expr
--               | ExprDigit Range Int
--               | ExprName        Name
--               | ExprLabel       Label
--               deriving (Show, Functor)
