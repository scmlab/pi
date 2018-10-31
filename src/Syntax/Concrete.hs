{-# LANGUAGE DeriveGeneric #-}

module Syntax.Concrete where
import qualified Syntax.Primitive as P

import Control.Monad.Except
import Data.Text (Text, unpack)

--------------------------------------------------------------------------------
-- | Concrete Syntax Tree

data Point = Point Int Int Int  -- row / column / index
  deriving (Show)
data Range = Range Point Point Text -- start / end / text
  deriving (Show)


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

--------------------------------------------------------------------------------
-- | Converting from Primivite Syntax Tree

data ParseError = ParseError Range String Text deriving (Show)

parsePrim :: P.SyntaxTree -> Either ParseError Program
parsePrim = runExcept . fromPrim

class FromPrim a where
  fromPrim :: P.SyntaxTree -> Except ParseError a

-- access the nth child safely
pickChild :: P.SyntaxTree -> Int -> Except ParseError P.SyntaxTree
pickChild node@(P.Node _ children _ _ _ _) i = do
  if i >= length children then
    fromPrim node >>= throwError
  else
    return (children !! i)

fromPrimChild :: FromPrim a => P.SyntaxTree -> Int -> Except ParseError a
fromPrimChild node i = do
  pickChild node i >>= fromPrim

-- returns the kind of the nth child
kindOfChild :: P.SyntaxTree -> Int -> Except ParseError String
kindOfChild node i = do
  P.kind <$> pickChild node i

instance FromPrim ParseError where
  fromPrim node@(P.Node expected _ got _ _ _) = do
    range <- fromPrim node
    throwError $ ParseError range expected got

instance FromPrim Range where
  fromPrim (P.Node _ _ text (P.RangePrim (P.PointPrim a b) (P.PointPrim c d)) start end) =
    return $ Range (Point a b start) (Point c d end) text

instance FromPrim Label where
  fromPrim node@(P.Node "label" _ text _ _ _) =
    Label
      <$> fromPrim node
      <*> (return text)
  fromPrim node = fromPrim node >>= throwError

instance FromPrim Name where
  fromPrim node@(P.Node "reserved_name" _ text _ _ _) =
    Reserved
      <$> fromPrim node
      <*> (return text)
  fromPrim node@(P.Node "name" _ text _ _ _) =
    Name
      <$> fromPrim node
      <*> (return text)
  fromPrim node@(P.Node "process_name" _ text _ _ _) =
    Name
      <$> fromPrim node
      <*> (return text)
  fromPrim node = fromPrim node >>= throwError

instance FromPrim Program where
  fromPrim node@(P.Node "program" children _ _ _ _) =
    Program
      <$> fromPrim node
      <*> mapM fromPrim children
  fromPrim node = fromPrim node >>= throwError

instance FromPrim ProcDecl where
  fromPrim node@(P.Node "proc_declaration" _ _ _ _ _) =
    ProcDecl
      <$> fromPrim node
      <*> fromPrimChild node 0
      <*> fromPrimChild node 1
  fromPrim node = fromPrim node >>= throwError

instance FromPrim Process where
  fromPrim node@(P.Node "nu" _ _ _ _ _) =
    Nu
      <$> fromPrim node
      <*> fromPrimChild node 0
      <*> fromPrimChild node 1
  fromPrim node@(P.Node "send" _ _ _ _ _) =
    Send
      <$> fromPrim node
      <*> fromPrimChild node 0
      <*> fromPrimChild node 1
      <*> fromPrimChild node 2
  fromPrim node@(P.Node "recv" children _ _ _ _) = do
    Recv
      <$> fromPrim node
      <*> fromPrimChild node 0
      <*> mapM fromPrim (tail children)
  fromPrim node@(P.Node "par" _ _ _ _ _) =
    Par
      <$> fromPrim node
      <*> fromPrimChild node 0
      <*> fromPrimChild node 1
  fromPrim node@(P.Node "call" _ _ _ _ _) =
    Call
    <$> fromPrim node
    <*> fromPrimChild node 0
  fromPrim node@(P.Node "end" _ _ _ _ _) =
    End
      <$> fromPrim node
  fromPrim node = fromPrim node >>= throwError

instance FromPrim Pattern where
  fromPrim node@(P.Node "pattern" _ _ _ _ _) = do
    kind <- kindOfChild node 0
    case kind of
      "name"  -> PtrnName  <$> fromPrimChild node 0
      "label" -> PtrnLabel <$> fromPrimChild node 0
      _       -> fromPrim node >>= throwError
  fromPrim node = fromPrim node >>= throwError

instance FromPrim Clause where
  fromPrim node@(P.Node "clause" _ _ _ _ _) =
    Clause
      <$> fromPrim node
      <*> fromPrimChild node 0
      <*> fromPrimChild node 1
  fromPrim node = fromPrim node >>= throwError

instance FromPrim Expr where
  fromPrim node@(P.Node "mul" _ _ _ _ _) =
    Mul
      <$> fromPrim node
      <*> fromPrimChild node 0
      <*> fromPrimChild node 1
  fromPrim node@(P.Node "div" _ _ _ _ _) =
    Div
      <$> fromPrim node
      <*> fromPrimChild node 0
      <*> fromPrimChild node 1
  fromPrim node@(P.Node "add" _ _ _ _ _) =
    Add
      <$> fromPrim node
      <*> fromPrimChild node 0
      <*> fromPrimChild node 1
  fromPrim node@(P.Node "sub" _ _ _ _ _) =
    Sub
      <$> fromPrim node
      <*> fromPrimChild node 0
      <*> fromPrimChild node 1
  fromPrim node@(P.Node "integer" _ text _ _ _) =
    ExprDigit
      <$> fromPrim node
      <*> (return $ read $ unpack text)
  fromPrim node@(P.Node "variable" _ _ _ _ _) =
    ExprName
      <$> fromPrimChild node 0
  fromPrim node@(P.Node "label" _ _ _ _ _) =
    ExprLabel
      <$> fromPrim node
  fromPrim node = fromPrim node >>= throwError
