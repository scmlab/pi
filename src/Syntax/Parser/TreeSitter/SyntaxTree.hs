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
      <$> toConcrete node
      <*> (return text)
  toConcrete node = toConcrete node >>= throwError

instance ToConcrete ann => ToConcrete (Name ann) where
  toConcrete node@(Node "reserved_name" _ text _ _ _) =
    Reserved
      <$> toConcrete node
      <*> (return text)
  toConcrete node@(Node "name" _ text _ _ _) =
    Name
      <$> toConcrete node
      <*> (return text)
  toConcrete node@(Node "process_name" _ text _ _ _) =
    Name
      <$> toConcrete node
      <*> (return text)
  toConcrete node = toConcrete node >>= throwError

instance ToConcrete ann => ToConcrete (Program ann) where
  toConcrete node@(Node "program" children _ _ _ _) =
    Program
      <$> toConcrete node
      <*> mapM toConcrete children
  toConcrete node = toConcrete node >>= throwError

instance ToConcrete ann => ToConcrete (ProcDecl ann) where
  toConcrete node@(Node "proc_declaration" _ _ _ _ _) =
    ProcDecl
      <$> toConcrete node
      <*> toConcreteChild node 0
      <*> toConcreteChild node 1
  toConcrete node = toConcrete node >>= throwError

instance ToConcrete ann => ToConcrete (Process ann) where
  toConcrete node@(Node "nu" _ _ _ _ _) =
    Nu
      <$> toConcrete node
      <*> toConcreteChild node 0
      <*> toConcreteChild node 1
  toConcrete node@(Node "send" _ _ _ _ _) =
    Send
      <$> toConcrete node
      <*> toConcreteChild node 0
      <*> toConcreteChild node 1
      <*> toConcreteChild node 2
  toConcrete node@(Node "recv" children _ _ _ _) = do
    Recv
      <$> toConcrete node
      <*> toConcreteChild node 0
      <*> mapM toConcrete (tail children)
  toConcrete node@(Node "par" _ _ _ _ _) =
    Par
      <$> toConcrete node
      <*> toConcreteChild node 0
      <*> toConcreteChild node 1
  toConcrete node@(Node "call" _ _ _ _ _) =
    Call
    <$> toConcrete node
    <*> toConcreteChild node 0
  toConcrete node@(Node "end" _ _ _ _ _) =
    End
      <$> toConcrete node
  toConcrete node = toConcrete node >>= throwError

instance ToConcrete ann => ToConcrete (Pattern ann) where
  toConcrete node@(Node "pattern" _ _ _ _ _) = do
    kind <- kindOfChild node 0
    case kind of
      "name"  -> PtrnName
                  <$> toConcrete node
                  <*> toConcreteChild node 0
      "label" -> PtrnLabel
                  <$> toConcrete node
                  <*> toConcreteChild node 0
      _       -> toConcrete node >>= throwError
  toConcrete node = toConcrete node >>= throwError

instance ToConcrete ann => ToConcrete (Clause ann) where
  toConcrete node@(Node "clause" _ _ _ _ _) =
    Clause
      <$> toConcrete node
      <*> toConcreteChild node 0
      <*> toConcreteChild node 1
  toConcrete node = toConcrete node >>= throwError

instance ToConcrete ann => ToConcrete (Expr ann) where
  toConcrete node@(Node "mul" _ _ _ _ _) =
    Mul
      <$> toConcrete node
      <*> toConcreteChild node 0
      <*> toConcreteChild node 1
  toConcrete node@(Node "div" _ _ _ _ _) =
    Div
      <$> toConcrete node
      <*> toConcreteChild node 0
      <*> toConcreteChild node 1
  toConcrete node@(Node "add" _ _ _ _ _) =
    Add
      <$> toConcrete node
      <*> toConcreteChild node 0
      <*> toConcreteChild node 1
  toConcrete node@(Node "sub" _ _ _ _ _) =
    Sub
      <$> toConcrete node
      <*> toConcreteChild node 0
      <*> toConcreteChild node 1
  toConcrete node@(Node "integer" _ text _ _ _) =
    ExprDigit
      <$> toConcrete node
      <*> (return $ read $ unpack text)
  toConcrete node@(Node "variable" _ _ _ _ _) =
    ExprName
      <$> toConcrete node
      <*> toConcreteChild node 0
  toConcrete node@(Node "label" _ _ _ _ _) =
    ExprLabel
      <$> toConcrete node
      <*> toConcreteChild node 0
  toConcrete node = toConcrete node >>= throwError
