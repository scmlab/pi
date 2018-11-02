module Syntax.Parser.Error where

import Data.Text (Text)

data Point = Point Int Int Int  -- row / column / index
  deriving (Show, Eq)
data Range = Range Point Point Text -- start / end / text
  deriving (Show, Eq)


data HaskellErrClass
  = Syntactical (Maybe String)
  | Lexical
  | Message String
  deriving (Show, Eq)

data ParseError
  = HaskellParseError
  { errLine  :: Int
  , errPos   :: Int
  , errClass :: HaskellErrClass
  }
  | TreeSitterParseError Range String Text
  deriving (Show, Eq)
