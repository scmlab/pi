{-# LANGUAGE DeriveGeneric #-}

module Syntax.Primitive where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)

--------------------------------------------------------------------------------
-- | Primitive Syntax Tree (from the tree-sitter parsers)

data PointPrim = PointPrim
  { row     :: Int
  , column  :: Int
  } deriving (Show, Generic)

data RangePrim = RangePrim
  { start :: PointPrim
  , end   :: PointPrim
  } deriving (Show, Generic)

data SyntaxTree = Node
  { kind        :: String
  , children    :: [SyntaxTree]
  , text        :: Text
  , range       :: RangePrim
  , startIndex  :: Int
  , endIndex    :: Int
  } deriving (Show, Generic)

instance FromJSON PointPrim where
instance FromJSON RangePrim where
instance FromJSON SyntaxTree where
