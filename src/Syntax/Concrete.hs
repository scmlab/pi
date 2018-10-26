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

data Name     = Name      Range Text          deriving (Show)

data Program  = Program   Range [ProcDecl]    deriving (Show)
data ProcDecl = ProcDecl  Range Name Process  deriving (Show)

data Process  = Send      Range Name Expr Process
              | Recv      Range Name Name Process
              | Nu        Range Name      Process
              | Par       Range           Process Process
              | End       Range
              deriving (Show)

-- Expressions and all that
data Expr     = Mul       Range Expr Expr
              | Div       Range Expr Expr
              | Add       Range Expr Expr
              | Minus     Range Expr Expr
              | Digit     Range Int
              deriving (Show)

--------------------------------------------------------------------------------
-- | Converting from Primivite Syntax Tree

parsePrim :: P.SyntaxTree -> Either Range Program
parsePrim = runExcept . fromPrim

class FromPrim a where
  fromPrim :: P.SyntaxTree -> Except Range a

instance FromPrim Range where
  fromPrim (P.Node _ _ text (P.RangePrim (P.PointPrim a b) (P.PointPrim c d)) start end) =
    return $ Range (Point a b start) (Point c d end) text

instance FromPrim Name where
  fromPrim node@(P.Node "name" _ text _ _ _) =
    Name
      <$> fromPrim node
      <*> (return text)
  fromPrim node = fromPrim node >>= throwError

instance FromPrim Program where
  fromPrim node@(P.Node "source_file" children _ _ _ _) =
    Program
      <$> fromPrim node
      <*> mapM fromPrim children
  fromPrim node = fromPrim node >>= throwError

instance FromPrim ProcDecl where
  fromPrim node@(P.Node "proc_declaration" children _ _ _ _) =
    ProcDecl
      <$> fromPrim node
      <*> fromPrim (children !! 0)
      <*> fromPrim (children !! 2)
  fromPrim node = fromPrim node >>= throwError

instance FromPrim Process where
  fromPrim node@(P.Node "nu" children _ _ _ _) =
    Nu
      <$> fromPrim node
      <*> fromPrim (children !! 2)
      <*> fromPrim (children !! 4)
  fromPrim node@(P.Node "send" children _ _ _ _) =
    Send
      <$> fromPrim node
      <*> fromPrim (children !! 0)
      <*> fromPrim (children !! 2)
      <*> fromPrim (children !! 4)
  fromPrim node@(P.Node "recv" children _ _ _ _) =
    Recv
      <$> fromPrim node
      <*> fromPrim (children !! 0)
      <*> fromPrim (children !! 2)
      <*> fromPrim (children !! 4)
  fromPrim node@(P.Node "par" children _ _ _ _) =
    Par
      <$> fromPrim node
      <*> fromPrim (children !! 0)
      <*> fromPrim (children !! 2)
  fromPrim node@(P.Node "end" children _ _ _ _) = End <$> fromPrim node
  fromPrim node = fromPrim node >>= throwError

instance FromPrim Expr where
  fromPrim node@(P.Node "expr_expr" children _ _ _ _) =
    fromPrim (children !! 1)  -- wrapped in a pair of ( )
  fromPrim node@(P.Node "expr_mul" children _ _ _ _) =
    Mul
      <$> fromPrim node
      <*> fromPrim (children !! 0)
      <*> fromPrim (children !! 2)
  fromPrim node@(P.Node "expr_div" children _ _ _ _) =
    Div
      <$> fromPrim node
      <*> fromPrim (children !! 0)
      <*> fromPrim (children !! 2)
  fromPrim node@(P.Node "expr_factor" children _ _ _ _) =
    fromPrim (children !! 0)  -- go down
  fromPrim node@(P.Node "factor_expr" children _ _ _ _) =
    fromPrim (children !! 1)  -- wrapped in a pair of ( )
  fromPrim node@(P.Node "factor_add" children _ _ _ _) =
    Add
      <$> fromPrim node
      <*> fromPrim (children !! 0)
      <*> fromPrim (children !! 2)
  fromPrim node@(P.Node "factor_minus" children _ _ _ _) =
    Minus
      <$> fromPrim node
      <*> fromPrim (children !! 0)
      <*> fromPrim (children !! 2)
  fromPrim node@(P.Node "factor_term" children _ _ _ _) =
    fromPrim (children !! 0)  -- go down
  fromPrim node@(P.Node "term_expr" children _ _ _ _) =
    fromPrim (children !! 1)  -- wrapped in a pair of ( )
  fromPrim node@(P.Node "term_digit" children text _ _ _) =
    Digit
      <$> fromPrim node
      <*> (return $ read $ unpack text)
  fromPrim node = fromPrim node >>= throwError
