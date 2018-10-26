{-# LANGUAGE OverloadedStrings #-}

module Interaction.JSON where


import Data.Aeson
import Data.Text hiding (pack)
import Data.ByteString.Char8 (getLine, putStrLn, pack)
import Prelude hiding (getLine, putStrLn)
import qualified Syntax.Primitive as Prim
import qualified Syntax.Concrete as Conc
import qualified Syntax.Abstract as Abst

import Interaction
import Syntax.Abstract

--------------------------------------------------------------------------------
-- | Interfacing with Machines

jsonREPL :: IO ()
jsonREPL = loop
  where
    loop :: IO ()
    loop = do
      raw <- getLine
      case (eitherDecodeStrict raw :: Either String Request) of
        Left err -> putStrLn $ pack $ show err
        Right val -> putStrLn $ pack $ show val
      loop

--------------------------------------------------------------------------------
-- | Encoding & Decoding JSON

instance FromJSON Request where
  parseJSON = withObject "Request" $ \obj -> do
    kind <- obj .: "request"
    case (kind :: Text) of
      "load"  -> do
        pst <- obj .: "syntax-tree"
        case Conc.parsePrim pst of
          Left err -> return $ Err (show err)
          Right program -> return $ Load (Abst.fromConcrete program)
      "run"   -> do
        i <- obj .: "index"
        return $ Run i
      "feed"  -> do
        i <- obj .: "index"
        v <- obj .: "value"
        return $ Feed i (VI v)
      _       -> fail "unknown kind"
