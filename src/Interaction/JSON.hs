{-# LANGUAGE OverloadedStrings #-}

module Interaction.JSON where

import Control.Monad.State hiding (State, state)
import Control.Monad.Except
import Data.Aeson
import Data.Text hiding (pack, map)
import Data.ByteString.Char8 (getLine, putStrLn, pack)
import Data.ByteString.Lazy.Char8 (toStrict)
import Prelude hiding (getLine, putStrLn)
import qualified Syntax.Abstract as Abst

import Data.Text.Prettyprint.Doc (pretty)

import Interaction
import Interpreter
import Syntax.Abstract
import qualified Syntax.Concrete as Conc

--------------------------------------------------------------------------------
-- | Interfacing with Machines

jsonREPL :: IO ()
jsonREPL = do
  (_, _) <- runInteraction [] (Call (NS "main")) loop
  return ()

  where
    response :: ToJSON a => a -> InteractionM IO ()
    response = liftIO . putStrLn . toStrict . encode

    loop :: InteractionM IO ()
    loop = do
      raw <- liftIO getLine
      case (eitherDecodeStrict raw :: Either String Request) of
        Left err -> response $ ResGenericError err
        Right req -> case req of
          Err err -> do
            response $ ResParseError err
          Test -> do
            state <- get
            response $ ResTest (show state)
          Load (Prog prog) -> do
            load $ map (\(PiDecl name p) -> (name, p)) prog
            gets stateOutcomes >>= response . ResOutcomes
          Run i -> do
            try (run i)
            gets stateOutcomes >>= response . ResOutcomes
          Feed i _ -> do
            try (run i)
            gets stateOutcomes >>= response . ResOutcomes
      loop

    try :: InteractionM IO () -> InteractionM IO ()
    try program = do
      program `catchError` (liftIO . putStrLn . pack . show)

--------------------------------------------------------------------------------
-- | Encoding & Decoding JSON

instance FromJSON Request where
  parseJSON = withObject "Request" $ \obj -> do
    kind <- obj .: "request"
    case (kind :: Text) of
      "load"  -> do
        pst <- obj .: "syntax-tree"
        case Conc.parsePrim pst of
          Left err      -> return $ Err err
          Right program -> return $ Load (Abst.fromConcrete program)
      "test"   -> return $ Test
      "run"   -> do
        i <- obj .: "index"
        return $ Run i
      "feed"  -> do
        i <- obj .: "index"
        v <- obj .: "value"
        return $ Feed i (VI v)
      _       -> fail "unknown kind"

instance ToJSON Response where
  toJSON (ResOutcomes outcomes) = object
    [ "response" .= ("outcomes" :: Text)
    , "payload"  .= outcomes
    ]
  toJSON (ResTest payload) = object
    [ "response" .= ("test" :: Text)
    , "payload"  .= payload
    ]
  toJSON (ResParseError err) = object
    [ "response" .= ("parse-error" :: Text)
    , "payload"  .= err
    ]
  toJSON (ResGenericError err) = object
    [ "response" .= ("generic-error" :: Text)
    , "payload"  .= err
    ]

instance ToJSON Conc.ParseError where
  toJSON (Conc.ParseError range expected got) = object
    [ "range"     .= show range
    , "expected"  .= expected
    , "got"       .= got
    ]

instance ToJSON Reaction where
  toJSON (Silent state) = object
    [ "kind"  .= ("silent" :: Text)
    , "state" .= state
    ]
  toJSON (React state channel sender receiver products) = object
    [ "kind"      .= ("react" :: Text)
    , "state"     .= state
    , "channel"   .= channel
    , "sender"    .= sender
    , "receiver"  .= receiver
    , "products"  .= products
    ]
  toJSON (Output state sender) = object
    [ "kind"      .= ("output" :: Text)
    , "state"     .= state
    , "sender"    .= sender
    ]
  toJSON (Input state receiver) = object
    [ "kind"      .= ("input" :: Text)
    , "state"     .= state
    , "receiver"  .= receiver
    ]

instance ToJSON St where
  toJSON (St senders receivers blocked _) = object
    [ "senders"     .= senders
    , "receivers"   .= receivers
    , "blocked"     .= blocked
    ]

instance ToJSON Name where
  toJSON = toJSON . show . pretty

instance ToJSON Val where
  toJSON = toJSON . show . pretty

instance ToJSON Pi where
  toJSON = toJSON . show . pretty

instance ToJSON Ptrn where
  toJSON = toJSON . show . pretty

instance ToJSON Sender where
  toJSON (Sender value process) = object
    [ "value"     .= value
    , "process"   .= process
    ]

instance ToJSON Receiver where
  toJSON (Receiver clauses) = toJSON clauses

instance ToJSON Clause where
  toJSON (Clause pattern process) = object
    [ "pattern"   .= pattern
    , "process"   .= process
    ]

-- instance ToJSON Val where
--   toJSON
