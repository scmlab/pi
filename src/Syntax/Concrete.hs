module Syntax.Concrete where

import Syntax.Parser.Type
import Data.Text (Text)

--------------------------------------------------------------------------------
-- | Concrete Syntax Tree

data Label    = Label     Range Text          deriving (Show)
data Name     = Name      Range Text
              | Reserved  Range Text          deriving (Show)

data Program  = Program   Range [ProcDecl]    deriving (Show)
data ProcDecl = ProcDecl  Range Name Process  deriving (Show)

data Process  = Send      Range Name Expr Process
              | Recv      Range Name      [Clause]
              | Nu        Range Name      Process
              | Par       Range           Process Process
              | Call      Range Name
              | End       Range
              deriving (Show)

data Pattern  = PtrnName  Name
              | PtrnLabel Label
              deriving (Show)
data Clause   = Clause    Range Pattern Process        deriving (Show)

-- Expressions and all that
data Expr     = Mul       Range Expr Expr
              | Div       Range Expr Expr
              | Add       Range Expr Expr
              | Sub       Range Expr Expr
              | ExprDigit Range Int
              | ExprName        Name
              | ExprLabel       Label
              deriving (Show)
