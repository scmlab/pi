{-# LANGUAGE DeriveGeneric #-}

module Syntax.Parser.Type where

import Data.Text (Text)
import Data.Aeson (FromJSON)
import GHC.Generics

-- data Point = Point Int Int Int  -- offset / line / column
--   deriving (Show, Eq)
-- data Range = Range Point Point Text -- start / end / text
--   deriving (Show, Eq)
--
--
-- data HaskellErrClass
--   = Syntactical (Maybe String)
--   | Lexical
--   | Message String
--   deriving (Show, Eq)
--
-- data ParseError
--   = HaskellParseError
--   { errLine  :: Int
--   , errPos   :: Int
--   , errClass :: HaskellErrClass
--   }
--   | TreeSitterParseError Range String Text
--   | RequestParseError Text
--   deriving (Show, Eq)
--
-- --------------------------------------------------------------------------------
-- -- | Syntax Tree (from the tree-sitter parser)
--
-- data PointST = PointST
--   { stRow     :: Int
--   , stColumn  :: Int
--   } deriving (Show, Generic)
--
-- data RangeST = RangeST
--   { stStart :: PointST
--   , stEnd   :: PointST
--   } deriving (Show, Generic)
--
-- data SyntaxTree = Node
--   { stKind        :: String
--   , stChildren    :: [SyntaxTree]
--   , stText        :: Text
--   , stRange       :: RangeST
--   , stStartIndex  :: Int
--   , stEndIndex    :: Int
--   } deriving (Show, Generic)
--
-- instance FromJSON PointST where
-- instance FromJSON RangeST where
-- instance FromJSON SyntaxTree where
