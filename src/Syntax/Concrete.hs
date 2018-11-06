module Syntax.Concrete where

import Data.Text (Text)

--------------------------------------------------------------------------------
-- | Concrete Syntax Tree

data Label    ann = Label     ann Text                        deriving (Show)
data Name     ann = Name      ann Text
                  | Reserved  ann Text                        deriving (Show)

data Program  ann = Program   ann [ProcDecl ann]              deriving (Show)
data ProcDecl ann = ProcDecl  ann (Name ann)    (Process ann) deriving (Show)

data Process  ann = Send      ann (Name ann)    (Expr ann)    (Process ann)
                  | Recv      ann (Name ann)    [Clause ann]
                  | Nu        ann (Name ann)    (Process ann)
                  | Par       ann (Process ann) (Process ann)
                  | Call      ann (Name ann)
                  | End       ann
                  deriving (Show)

data Pattern  ann = PtrnName  ann (Name ann)
                  | PtrnLabel ann (Label ann)
                  deriving (Show)
data Clause   ann = Clause    ann (Pattern ann) (Process ann)
                  deriving (Show)

-- Expressions and all that
data Expr     ann = Mul       ann (Expr ann) (Expr ann)
                  | Div       ann (Expr ann) (Expr ann)
                  | Add       ann (Expr ann) (Expr ann)
                  | Sub       ann (Expr ann) (Expr ann)
                  | ExprDigit ann Int
                  | ExprName  ann (Name ann)
                  | ExprLabel ann (Label ann)
                  deriving (Show)


-- data Label    = Label     Range Text          deriving (Show)
-- data Name     = Name      Range Text
--               | Reserved  Range Text          deriving (Show)
--
-- data Program  = Program   Range [ProcDecl]    deriving (Show)
-- data ProcDecl = ProcDecl  Range Name Process  deriving (Show)
--
-- data Process  = Send      Range Name Expr Process
--               | Recv      Range Name      [Clause]
--               | Nu        Range Name      Process
--               | Par       Range           Process Process
--               | Call      Range Name
--               | End       Range
--               deriving (Show)
--
-- data Pattern  = PtrnName  Name
--               | PtrnLabel Label
--               deriving (Show)
-- data Clause   = Clause    Range Pattern Process        deriving (Show)
--
-- -- Expressions and all that
-- data Expr     = Mul       Range Expr Expr
--               | Div       Range Expr Expr
--               | Add       Range Expr Expr
--               | Sub       Range Expr Expr
--               | ExprDigit Range Int
--               | ExprName        Name
--               | ExprLabel       Label
--               deriving (Show)
