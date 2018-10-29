{-# LANGUAGE OverloadedStrings #-}

module Interaction.JSON where

import Control.Monad.State hiding (State)
import Control.Monad.Except
import Data.Aeson
import Data.Text hiding (pack, map)
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
jsonREPL = do
  (_, _) <- runInteraction initialEnv (Call (NS "main")) loop
  return ()

  where
    loop :: InteractionM IO ()
    loop = do
      raw <- liftIO getLine
      case (eitherDecodeStrict raw :: Either String Request) of
        Left err -> liftIO $ putStrLn $ pack $ show err
        Right req -> case req of
          Err err -> do
            liftIO $ putStrLn (pack $ show $ err)
          Test (Prog prog) -> do
            liftIO $ putStrLn (pack $ show $ prog)
          Load (Prog prog) -> do
            load $ map (\(PiDecl name p) -> (name, p)) prog
            printChoices
          Run i -> do
            try (run i)
            printChoices
          Feed i v -> do
            try (run i)
            printChoices
      loop

    printChoices :: InteractionM IO ()
    printChoices = gets choices >>= liftIO . putStrLn . pack . show . ppChoices

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
          Left err      -> return $ Err (show err)
          Right program -> return $ Load (Abst.fromConcrete program)
      "test"   -> do
        pst <- obj .: "syntax-tree"
        case Conc.parsePrim pst of
          Left err      -> return $ Err (show err)
          Right program -> return $ Test (Abst.fromConcrete program)
      "run"   -> do
        i <- obj .: "index"
        return $ Run i
      "feed"  -> do
        i <- obj .: "index"
        v <- obj .: "value"
        return $ Feed i (VI v)
      _       -> fail "unknown kind"
