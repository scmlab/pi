{-# LANGUAGE OverloadedStrings #-}

module Interaction.JSON where

import Prelude hiding (getLine, putStrLn)
import Interaction
import Data.Aeson
import Data.Text
import Data.ByteString.Char8 (getLine, putStrLn)

import Syntax

--------------------------------------------------------------------------------
-- | Interfacing with Machines

jsonREPL :: IO ()
jsonREPL = loop
  where
    loop :: IO ()
    loop = do
      raw <- getLine
      case (decodeStrict raw :: Maybe Request) of
        Just req -> putStrLn "success"
        Nothing -> putStrLn "failed parsing requests"
      loop

-- recvRequest ::

-- parseRequest :: ByteString -> Maybe
-- parseRequest raw = do

--------------------------------------------------------------------------------
-- | Encoding & Decoding JSON

instance FromJSON Request where
  parseJSON = withObject "Request" $ \obj -> do
    kind <- obj .: "kind"
    case (kind :: Text) of
      "run"   -> do
        i <- obj .: "index"
        return $ Run i
      "feed"  -> do
        i <- obj .: "index"
        v <- obj .: "value"
        return $ Feed i (VI v)
      _       -> fail "unknown kind"
