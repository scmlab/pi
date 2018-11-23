{-# LANGUAGE OverloadedStrings #-}

module Interaction.Human where

import Control.Monad.State hiding (State, state)
import Control.Monad.Except
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import Data.Text.Prettyprint.Doc (pretty)
import Prelude hiding (readFile)
import System.Console.Haskeline
import System.IO

import Interaction
import Syntax.Parser (printParseError)

--------------------------------------------------------------------------------
-- | Interfacing with Humans

humanREPL :: [FilePath] -> IO ()
humanREPL [] = void $ runInteraction $ do
  displayHelp
  loop
humanREPL (filePath:_) = void $ runInteraction $ do
  handleError $ handleRequest (Load (trim filePath))
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
  liftIO getKey >>= handleError . handleRequest . parseRequest
  loop

printStatusBar :: InteractionM IO ()
printStatusBar = do
  outcomes <- gets stOutcomes
  cursor <- gets stCursor
  case cursor of
    Nothing -> liftIO $ putStrLn $ "0/0 outcomes"
    Just n  -> liftIO $ putStrLn $ show (n + 1) ++ "/" ++ show (length outcomes) ++ " outcomes"

handleError :: InteractionM IO () -> InteractionM IO ()
handleError program = program `catchError` \ err -> case err of
  ParseError parseError -> gets stSource >>= liftIO . printParseError parseError
  TypeError msg -> liftIO (putStrLn (show msg))
  RuntimeError msg -> liftIO (putStrLn (show msg))
  InteractionError msg -> liftIO (putStrLn (show msg))

handleRequest :: Request -> InteractionM IO ()
handleRequest (CursorMoveTo n)  = do
  choose n
  outcome <- currentOutcome
  liftIO $ putStrLn $ show $ pretty outcome
  printStatusBar
handleRequest CursorUp          = withCursor $ \n -> do
  choose (n - 1)
  outcome <- currentOutcome
  liftIO $ putStrLn $ show $ pretty outcome
  printStatusBar
handleRequest CursorDown        = withCursor $ \n -> do
  choose (n + 1)
  outcome <- currentOutcome
  liftIO $ putStrLn $ show $ pretty outcome
  printStatusBar

handleRequest CursorNext        = do
  run
  currentOutcome >>= liftIO . putStrLn . show . pretty
  printStatusBar
handleRequest (Load filePath)   = do
  load filePath
  currentState >>= liftIO . putStrLn . show . pretty
  currentOutcome >>= liftIO . putStrLn . show . pretty
  printStatusBar
handleRequest Reload            = do
  reload
  currentState >>= liftIO . putStrLn . show . pretty
  currentOutcome >>= liftIO . putStrLn . show . pretty
  printStatusBar
handleRequest Test = do
  test
handleRequest Help              = displayHelp
handleRequest CursorPrev        = displayHelp
--------------------------------------------------------------------------------
-- | Parsing human input

parseRequest :: String -> Request
parseRequest key
  | "l"    `isPrefixOf` key = (Load . trim . drop 1) key
  | "load" `isPrefixOf` key = (Load . trim . drop 4) key
  | otherwise = case trim key of
      "\ESC[A"  -> CursorUp
      "\ESC[B"  -> CursorDown
      "\ESC[C"  -> CursorNext
      "\n"      -> CursorNext
      "h"       -> Help
      "help"    -> Help
      "r"       -> Reload
      "reload"  -> Reload
      "t"       -> Test
      "test"    -> Test
      _         -> Help

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
