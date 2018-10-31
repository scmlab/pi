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

data Name     = Name      Range Text
              | Reserved  Range Text          deriving (Show)

data Program  = Program   Range [ProcDecl]    deriving (Show)
data ProcDecl = ProcDecl  Range Name Process  deriving (Show)

data Process  = Send      Range Name Expr Process
              | Recv      Range Name Name Process
              | Nu        Range Name      Process
              | Par       Range           Process Process
              | Call      Range Name
              | End       Range
              deriving (Show)

-- Expressions and all that
data Expr     = Mul       Range Expr Expr
              | Div       Range Expr Expr
              | Add       Range Expr Expr
              | Sub       Range Expr Expr
              | Digit     Range Int
              | Var       Range Text
              deriving (Show)

--------------------------------------------------------------------------------
-- | Converting from Primivite Syntax Tree

data ParseError = ParseError Range String Text deriving (Show)

parsePrim :: P.SyntaxTree -> Either ParseError Program
parsePrim = runExcept . fromPrim

class FromPrim a where
  fromPrim :: P.SyntaxTree -> Except ParseError a

-- access the nth children safely
fromPrimChild :: FromPrim a => P.SyntaxTree -> Int -> Except ParseError a
fromPrimChild node@(P.Node _ children _ _ _ _) i = do
  if i >= length children then
    fromPrim node >>= throwError
  else
    fromPrim (children !! i)

instance FromPrim ParseError where
  fromPrim node@(P.Node expected _ got _ _ _) = do
    range <- fromPrim node
    throwError $ ParseError range expected got

instance FromPrim Range where
  fromPrim (P.Node _ _ text (P.RangePrim (P.PointPrim a b) (P.PointPrim c d)) start end) =
    return $ Range (Point a b start) (Point c d end) text

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
      <*> fromPrimChild node 2
  fromPrim node = fromPrim node >>= throwError

instance FromPrim Process where
  fromPrim node@(P.Node "nu" _ _ _ _ _) =
    Nu
      <$> fromPrim node
      <*> fromPrimChild node 2
      <*> fromPrimChild node 4
  fromPrim node@(P.Node "send" _ _ _ _ _) =
    Send
      <$> fromPrim node
      <*> fromPrimChild node 0
      <*> fromPrimChild node 2
      <*> fromPrimChild node 4
  fromPrim node@(P.Node "recv" _ _ _ _ _) =
    Recv
      <$> fromPrim node
      <*> fromPrimChild node 0
      <*> fromPrimChild node 2
      <*> fromPrimChild node 4
  fromPrim node@(P.Node "par" _ _ _ _ _) =
    Par
      <$> fromPrim node
      <*> fromPrimChild node 0
      <*> fromPrimChild node 2
  fromPrim node@(P.Node "call" _ _ _ _ _) =
    Call
    <$> fromPrim node
    <*> fromPrimChild node 0
  fromPrim node@(P.Node "end" _ _ _ _ _) =
    End
      <$> fromPrim node
  fromPrim node = fromPrim node >>= throwError

instance FromPrim Expr where
  fromPrim node@(P.Node "mul" _ _ _ _ _) =
    Mul
      <$> fromPrim node
      <*> fromPrimChild node 0
      <*> fromPrimChild node 2
  fromPrim node@(P.Node "div" _ _ _ _ _) =
    Div
      <$> fromPrim node
      <*> fromPrimChild node 0
      <*> fromPrimChild node 2
  fromPrim node@(P.Node "add" _ _ _ _ _) =
    Add
      <$> fromPrim node
      <*> fromPrimChild node 0
      <*> fromPrimChild node 2
  fromPrim node@(P.Node "sub" _ _ _ _ _) =
    Sub
      <$> fromPrim node
      <*> fromPrimChild node 0
      <*> fromPrimChild node 2
  fromPrim node@(P.Node "integer" _ text _ _ _) =
    Digit
      <$> fromPrim node
      <*> (return $ read $ unpack text)
  fromPrim node@(P.Node "variable" _ text _ _ _) =
    Var
      <$> fromPrim node
      <*> (return text)
  fromPrim node = fromPrim node >>= throwError
