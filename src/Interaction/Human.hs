{-# LANGUAGE OverloadedStrings #-}

module Interaction.Human where

import Control.Monad.State hiding (State, state)
import Control.Monad.Except
import Data.List (isPrefixOf)
import qualified Data.ByteString.Lazy as BS
-- import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (pretty)
import System.Console.Haskeline
import System.IO
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

import Interaction
-- import Interpreter (Reaction(..))
import Syntax.Abstract
import Syntax.Parser (parseByteString)
import Prelude hiding (readFile)

--------------------------------------------------------------------------------
-- | Interfacing with Humans

humanREPL :: [FilePath] -> IO ()
humanREPL [] = void $ runInteraction $ do
  displayHelp
  loop
humanREPL (filePath:_) = void $ runInteraction $ do
  handleRequest (Load filePath)
  loop

displayHelp :: InteractionM IO ()
displayHelp = liftIO $ do
  putStrLn "========================================"
  putStrLn "  arrow keys          for navigation"
  putStrLn "  :help               for this help message   (:h)"
  putStrLn "  :load FILEPATH      for loading files       (:l)"
  putStrLn "  :reload             for reloading           (:r)"
  putStrLn "========================================"

loop :: InteractionM IO ()
loop = do
  liftIO getKey >>= handleRequest . parseRequest
  loop

printStatusBar :: InteractionM IO ()
printStatusBar = do
  outcomes <- gets stOutcomes
  cursor <- gets stCursor
  case cursor of
    Nothing -> liftIO $ putStrLn $ "0/0 outcomes"
    Just n  -> liftIO $ putStrLn $ show (n + 1) ++ "/" ++ show (length outcomes) ++ " outcomes"

try :: InteractionM IO () -> InteractionM IO ()
try program = do
  program `catchError` \_ -> return ()
  -- program `catchError` (liftIO . putStrLn . show . pretty)

handleRequest :: Request -> InteractionM IO ()
handleRequest (CursorMoveTo n)  = do
    try $ choose n

    outcome <- retrieveOutcome
    liftIO $ putStrLn $ show $ pretty outcome
    printStatusBar
handleRequest CursorUp          = withCursor $ \n -> do
    try $ choose (n - 1)

    outcome <- retrieveOutcome
    liftIO $ putStrLn $ show $ pretty outcome
    printStatusBar
handleRequest CursorDown        = withCursor $ \n -> do
    try $ choose (n + 1)

    outcome <- retrieveOutcome
    liftIO $ putStrLn $ show $ pretty outcome
    printStatusBar

handleRequest CursorNext        = do
  try run
  -- currentState >>= liftIO . putStrLn . show . pretty
  retrieveOutcome >>= liftIO . putStrLn . show . pretty
  printStatusBar
handleRequest Help              = displayHelp
handleRequest (Load filePath)   = do
  rawFile <- liftIO $ BS.readFile filePath
  case parseByteString rawFile of
    Left err   -> (liftIO . putStrLn . show) err
    Right (Prog prog) -> do
      load filePath $ map (\(PiDecl name p) -> (name, p)) prog
      -- choose the first outcome and print its state
      state <- retrieveNthOutcome 0 >>= toState
      -- liftIO $ putStrLn $ show (length outcomes) ++ " possible outcomes"
      liftIO $ putStrLn $ show $ pretty state
      printStatusBar
handleRequest _                 = displayHelp

--------------------------------------------------------------------------------
-- | Parsing human input

parseRequest :: String -> Request
parseRequest key
  | "l "    `isPrefixOf` key = (Load . drop 2 . trim) key
  | "load " `isPrefixOf` key = (Load . drop 5 . trim) key
  | otherwise = case key of
      "\ESC[A"  -> CursorUp
      "\ESC[B"  -> CursorDown
      "\ESC[C"  -> CursorNext
      "\n"      -> CursorNext
      "h"       -> Help
      "help"    -> Help
      "r"       -> Reload
      "reload"  -> Reload
      _         -> error key
  where
    trim :: String -> String
    trim = dropWhileEnd isSpace . dropWhile isSpace

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
    _ -> reverse <$> interceptStdin [firstChar]

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
