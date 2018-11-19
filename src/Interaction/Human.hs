{-# LANGUAGE OverloadedStrings #-}

module Interaction.Human where

import Control.Monad.State hiding (State, state)
import Control.Monad.Except
import Data.List (isPrefixOf)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (pretty)
import System.Console.Haskeline
import System.IO
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

import Interaction
import Interpreter (Reaction(..))
import Syntax.Abstract
import Syntax.Parser (ParseError(..), parseByteString)
import Prelude hiding (readFile)

--------------------------------------------------------------------------------
-- | Interfacing with Humans

data Key = Up | Down | Next | Help | Load String | Other String

humanREPL :: [FilePath] -> IO ()
humanREPL [] = void $ runInteraction $ do
  displayHelp
  loop
humanREPL (filePath:_) = void $ runInteraction $ do
  parseFile filePath >>= handleRequest
  loop


displayHelp :: InteractionM IO ()
displayHelp = liftIO $ do
  putStrLn "========================================"
  putStrLn "  arrow keys          for navigation"
  putStrLn "  :help               for this help message   (:h)"
  putStrLn "  :load FILEPATH      for loading files       (:l)"
  putStrLn "========================================"

loop :: InteractionM IO ()
loop = do
  liftIO getKey >>= keyToRequst . parseKey >>= handleRequest
  loop

parseKey :: String -> Key
parseKey key
  | "l "    `isPrefixOf` key = (Load . drop 2 . trim) key
  | "load " `isPrefixOf` key = (Load . drop 5 . trim) key
  | otherwise = case key of
      "\ESC[A"  -> Up
      "\ESC[B"  -> Down
      "\ESC[C"  -> Next
      "\n"      -> Next
      "h"       -> Help
      "help"    -> Help
      _         -> Other key
  where
    trim :: String -> String
    trim = dropWhileEnd isSpace . dropWhile isSpace

parseFile :: FilePath -> InteractionM IO Request
parseFile path = do
  rawFile <- liftIO $ BS.readFile path
  case parseByteString rawFile of
    Left err   -> return $ ReqParseErr err
    Right prog -> return $ ReqLoad prog

keyToRequst :: Key -> InteractionM IO Request
keyToRequst Help = displayHelp >> return ReqNoOp
keyToRequst (Load newPath) = parseFile newPath
keyToRequst Up   = withCursor (return . ReqChoose . (flip (-) 1))
keyToRequst Down = withCursor (return . ReqChoose . (flip (+) 1))
keyToRequst Next = do
  outcome <- retrieveOutcome
  case outcome of
    Success _ (Input _) _ -> do
      void $ error "not implemented yet"
      return $ ReqFeed (VI 3)
    Success _ _ _         -> return $ ReqRun
    Failure err           -> return $ ReqOtherErr err
keyToRequst (Other key) = return $ ReqParseErr $ RequestParseError (Text.pack key)

handleRequest :: Request -> InteractionM IO ()
handleRequest ReqNoOp = return ()
handleRequest (ReqChoose n) = try $ do
  choose n
  outcome <- retrieveOutcome
  liftIO $ putStrLn $ show $ pretty outcome
  printStatusBar
handleRequest ReqRun = do
  try run
  -- currentState >>= liftIO . putStrLn . show . pretty
  retrieveOutcome >>= liftIO . putStrLn . show . pretty
  printStatusBar
handleRequest (ReqLoad (Prog prog)) = do
  load $ map (\(PiDecl name p) -> (name, p)) prog
  -- choose the first outcome and print its state
  state <- retrieveNthOutcome 0 >>= toState
  -- liftIO $ putStrLn $ show (length outcomes) ++ " possible outcomes"
  liftIO $ putStrLn $ show $ pretty state
  printStatusBar
handleRequest (ReqFeed _) = void $ error "not implemented yet"
handleRequest (ReqParseErr msg) = liftIO $ putStrLn (show msg)
handleRequest (ReqOtherErr msg) = liftIO $ putStrLn (show msg)

printStatusBar :: InteractionM IO ()
printStatusBar = do
  outcomes <- gets stateOutcomes
  cursor <- gets stateCursor
  case cursor of
    Nothing -> liftIO $ putStrLn $ "0/0 outcomes"
    Just n  -> liftIO $ putStrLn $ show (n + 1) ++ "/" ++ show (length outcomes) ++ " outcomes"


getKey :: IO String
getKey = do
  controlStdin

  firstChar <- getChar
  key <- case firstChar of
    ':' -> do
      restoreStdin
      runInputT defaultSettings $ do
        minput <- getInputLine "π > "
        case minput of
            Nothing    -> lift getKey
            Just input -> return input
    _ -> reverse <$> interceptStdin ""

  restoreStdin
  return key

  where
    controlStdin :: IO ()
    controlStdin = do
      hSetBuffering stdin NoBuffering
      hSetEcho      stdin False

    restoreStdin :: IO ()
    restoreStdin = do
      hSetBuffering stdin LineBuffering
      hSetEcho      stdin True

    interceptStdin :: String -> IO String
    interceptStdin buffer = do
      char <- getChar
      more <- hReady stdin
      if more
        then interceptStdin (char:buffer)
        else return         (char:buffer)

try :: InteractionM IO () -> InteractionM IO ()
try program = do
  program `catchError` \_ -> return ()
  -- program `catchError` (liftIO . putStrLn . show . pretty)
