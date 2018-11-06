{-# LANGUAGE DeriveGeneric #-}

module Syntax.Parser.TreeSitter.SyntaxTree (parse) where

import Syntax.Parser.Type
import Syntax.Concrete

import Control.Monad.Except
import Data.Text (unpack)


--------------------------------------------------------------------------------
-- | Converting from Primivite Syntax Tree

parse :: SyntaxTree -> Either ParseError (Program Range)
parse = runExcept . toConcrete

class ToConcrete a where
  toConcrete :: SyntaxTree -> Except ParseError a

-- access the nth child safely
pickChild :: SyntaxTree -> Int -> Except ParseError SyntaxTree
pickChild node@(Node _ children _ _ _ _) i = do
  if i >= length children then
    toConcrete node >>= throwError
  else
    return (children !! i)

toConcreteChild :: ToConcrete a => SyntaxTree -> Int -> Except ParseError a
toConcreteChild node i = do
  pickChild node i >>= toConcrete

-- returns the kind of the nth child
kindOfChild :: SyntaxTree -> Int -> Except ParseError String
kindOfChild node i = do
  stKind <$> pickChild node i

instance ToConcrete ParseError where
  toConcrete node@(Node expected _ got _ _ _) = do
    range <- toConcrete node
    throwError $ TreeSitterParseError range expected got

instance ToConcrete Range where
  toConcrete (Node _ _ text (RangeST (PointST a b) (PointST c d)) start end) =
    return $ Range (Point a b start) (Point c d end) text

instance ToConcrete ann => ToConcrete (Label ann) where
  toConcrete node@(Node "label" _ text _ _ _) =
    Label
      <$> (return text)
      <*> toConcrete node
  toConcrete node = toConcrete node >>= throwError

instance ToConcrete ann => ToConcrete (Name ann) where
  toConcrete node@(Node "reserved_name" _ text _ _ _) =
    Reserved
      <$> (return text)
      <*> toConcrete node
  toConcrete node@(Node "name" _ text _ _ _) =
    Name
      <$> (return text)
      <*> toConcrete node
  toConcrete node@(Node "process_name" _ text _ _ _) =
    Name
      <$> (return text)
      <*> toConcrete node
  toConcrete node = toConcrete node >>= throwError

instance ToConcrete ann => ToConcrete (Program ann) where
  toConcrete node@(Node "program" children _ _ _ _) =
    Program
      <$> mapM toConcrete children
      <*> toConcrete node
  toConcrete node = toConcrete node >>= throwError

instance ToConcrete ann => ToConcrete (ProcDecl ann) where
  toConcrete node@(Node "proc_declaration" _ _ _ _ _) =
    ProcDecl
      <$> toConcreteChild node 0
      <*> toConcreteChild node 1
      <*> toConcrete node
  toConcrete node = toConcrete node >>= throwError

instance ToConcrete ann => ToConcrete (Process ann) where
  toConcrete node@(Node "nu" _ _ _ _ _) =
    Nu
      <$> toConcreteChild node 0
      <*> toConcreteChild node 1
      <*> toConcrete node
  toConcrete node@(Node "send" _ _ _ _ _) =
    Send
      <$> toConcreteChild node 0
      <*> toConcreteChild node 1
      <*> toConcreteChild node 2
      <*> toConcrete node
  toConcrete node@(Node "recv" children _ _ _ _) = do
    Recv
      <$> toConcreteChild node 0
      <*> mapM toConcrete (tail children)
      <*> toConcrete node
  toConcrete node@(Node "par" _ _ _ _ _) =
    Par
      <$> toConcreteChild node 0
      <*> toConcreteChild node 1
      <*> toConcrete node
  toConcrete node@(Node "call" _ _ _ _ _) =
    Call
      <$> toConcreteChild node 0
      <*> toConcrete node
  toConcrete node@(Node "end" _ _ _ _ _) =
    End
      <$> toConcrete node
  toConcrete node = toConcrete node >>= throwError

instance ToConcrete ann => ToConcrete (Pattern ann) where
  toConcrete node@(Node "pattern" _ _ _ _ _) = do
    kind <- kindOfChild node 0
    case kind of
      "name"  -> PtrnName
                  <$> toConcreteChild node 0
                  <*> toConcrete node
      "label" -> PtrnLabel
                  <$> toConcreteChild node 0
                  <*> toConcrete node
      _       -> toConcrete node >>= throwError
  toConcrete node = toConcrete node >>= throwError

instance ToConcrete ann => ToConcrete (Clause ann) where
  toConcrete node@(Node "clause" _ _ _ _ _) =
    Clause
      <$> toConcreteChild node 0
      <*> toConcreteChild node 1
      <*> toConcrete node
  toConcrete node = toConcrete node >>= throwError

instance ToConcrete ann => ToConcrete (Expr ann) where
  toConcrete node@(Node "mul" _ _ _ _ _) =
    Mul
      <$> toConcreteChild node 0
      <*> toConcreteChild node 1
      <*> toConcrete node
  toConcrete node@(Node "div" _ _ _ _ _) =
    Div
      <$> toConcreteChild node 0
      <*> toConcreteChild node 1
      <*> toConcrete node
  toConcrete node@(Node "add" _ _ _ _ _) =
    Add
      <$> toConcreteChild node 0
      <*> toConcreteChild node 1
      <*> toConcrete node
  toConcrete node@(Node "sub" _ _ _ _ _) =
    Sub
      <$> toConcreteChild node 0
      <*> toConcreteChild node 1
      <*> toConcrete node
  toConcrete node@(Node "integer" _ text _ _ _) =
    ExprDigit
      <$> (return $ read $ unpack text)
      <*> toConcrete node
  toConcrete node@(Node "variable" _ _ _ _ _) =
    ExprName
      <$> toConcreteChild node 0
      <*> toConcrete node
  toConcrete node@(Node "label" _ _ _ _ _) =
    ExprLabel
      <$> toConcreteChild node 0
      <*> toConcrete node
  toConcrete node = toConcrete node >>= throwError
