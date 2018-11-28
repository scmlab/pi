{-# LANGUAGE OverloadedStrings #-}

module Interaction.Human where

import Control.Monad.State hiding (State, state)
import Control.Monad.Except
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import Data.Text.Prettyprint.Doc
import Prelude hiding (readFile)
import System.Console.Haskeline
import System.IO
import System.Console.ANSI

import Interaction
-- import Interpreter (St(..))
import Interpreter
import Syntax.Abstract
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

printCurrentOutcome :: InteractionM IO ()
printCurrentOutcome = do
  outcome <- currentOutcome
  case outcome of
    Failure err -> throwError (InteractionError err)
    Success _ Silent _ -> do
      currentState >>= printState
      printStatusBar
    Success _ (Output (Sender v p)) _ -> do
      liftIO $ do
        setSGR [SetColor Foreground Vivid Yellow]
        putStrLn $ "Output"
        setSGR []
        print $ pretty v
      printStatusBar
    Success _ (React sender receiver _ _) _ -> do
      liftIO $ do
        setSGR [SetColor Foreground Vivid Yellow]
        putStrLn $ "React"
        setSGR []
      currentState >>= printState
      printStatusBar
    Success _ (Input _) _ -> do
      currentState >>= printState
      printStatusBar

-- printReact :: St -> Sender -> Receiver -> InteractionM IO ()
-- printReact (St senders receivers waitings freshVars) = do

printState :: St -> InteractionM IO ()
printState (St senders receivers waitings freshVars) = do

  let ss = [ Send c (EV v) p         | (c, (Sender v p))      <- senders ]
  let rs = [ Recv c          clauses | (c, Receiver clauses)  <- receivers ]
  let is = [ Recv (NR StdIn) clauses | (Receiver clauses)     <- waitings ]

  liftIO $ do
    when (not $ null ss) $ do
      setSGR [SetColor Foreground Dull Blue]
      putStrLn $ "  senders:"
      setSGR []
      putStrLn $ show $ indent 4 (vsep (map pretty ss))

    when (not $ null rs) $ do
      setSGR [SetColor Foreground Dull Blue]
      putStrLn $ "  receivers:"
      setSGR []
      putStrLn $ show $ indent 4 (vsep (map pretty rs))

    when (not $ null is) $ do
      setSGR [SetColor Foreground Dull Blue]
      putStrLn $ "  blocked:"
      setSGR []
      putStrLn $ show $ indent 4 (vsep (map pretty is))

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
  InteractionError msg -> do
    -- liftIO (putStr ("\r"))
    liftIO (putStrLn (show msg))
    -- liftIO (hFlush stdout)

handleRequest :: Request -> InteractionM IO ()
handleRequest (CursorMoveTo n)  = do
  choose n
  printCurrentOutcome
handleRequest CursorUp          = withCursor $ \n -> do
  choose (n - 1)
  printCurrentOutcome
handleRequest CursorDown        = withCursor $ \n -> do
  choose (n + 1)
  printCurrentOutcome
handleRequest CursorNext        = do
  run
  printCurrentOutcome
handleRequest (Load filePath)   = do
  load filePath
  printCurrentOutcome
handleRequest Reload            = do
  reload
  printCurrentOutcome
handleRequest Test = do
  test
handleRequest Help              = displayHelp
handleRequest CursorPrev        = displayHelp
--------------------------------------------------------------------------------
-- | Parsing human input

parseRequest :: String -> Request
parseRequest key
  | "load" `isPrefixOf` key = (Load . trim . drop 4) key
  | "l"    `isPrefixOf` key = (Load . trim . drop 1) key
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
