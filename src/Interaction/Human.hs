{-# LANGUAGE OverloadedStrings #-}

module Interaction.Human where

import Control.Monad.State hiding (State, state)
import Control.Monad.Except
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import Data.Text (unpack)
import Data.Text.Prettyprint.Doc
import Prelude hiding (readFile)
import System.Console.Haskeline
import System.IO
import Text.Read (readMaybe)

import Interaction
import Interaction.Scheduler
import Interaction.Util
-- import Interpreter (St(..))
import Interpreter
import Syntax.Abstract
import Syntax.Parser (printParseError)

--------------------------------------------------------------------------------
-- | Interfacing with Humans

humanREPL :: Bool -> [FilePath] -> IO ()
humanREPL _traceMode [] = void $ runInteraction $ do
  displayHelp
  loop
humanREPL True (filePath:_) = void $ runInteraction $ do
  handleError (handleRequest (Load filePath))
  loop
humanREPL False (filePath:_) = void $ runInteraction $ do
  handleError $ do
    load filePath
    execute

loop :: InteractionM IO ()
loop = do
  liftIO getKey >>= handleError . handleRequest . parseRequest
  loop

handleError :: InteractionM IO () -> InteractionM IO ()
handleError program = program `catchError` \ err -> case err of
  ParseError parseError -> gets stSource >>= liftIO . printParseError parseError
  TypeError msg -> liftIO (putStrLn (show msg))
  RuntimeError msg -> liftIO (putStrLn (show msg))
  InteractionError msg -> do
    -- liftIO (putStr ("\r"))
    liftIO (putStrLn (show msg))
    -- liftIO (hFlush stdout)

try :: InteractionM IO () -> InteractionM IO ()
try program = program `catchError` \_ -> return ()

handleRequest :: Request -> InteractionM IO ()
handleRequest (CursorMoveTo n)  = do
  choose n
  printFuture
handleRequest CursorPrev          = withCursor $ \n -> try $ do
  choose (n - 1)
  printFuture
handleRequest CursorNext        = withCursor $ \n -> try $ do
  choose (n + 1)
  printFuture
handleRequest CursorForth        = do
  run handleInput handleOutput
  skipSilent
  printFuture
  where
    handleInput :: InteractionM IO Val
    handleInput = do
      raw <- liftIO $ do
        yellow $ putStrLn $ "\nInput:"
        hFlush stdout
        -- requesting for input
        restoreStdin
        raw <- getLine
        controlStdin
        return raw
      -- parsing the input
      let result = VI <$> readMaybe raw
      case result of
        Just val -> return val
        Nothing -> do
          liftIO $ red $ putStrLn $ "cannot parse the input"
          handleInput

    handleOutput :: Val -> InteractionM IO ()
    handleOutput val = liftIO $ do
      yellow $ putStrLn $ "\nOutput:"
      green $ putStrLn $ show $ pretty val

    skipSilent :: InteractionM IO ()
    skipSilent = do
      next <- selectedFuture
      case next of
        Success _ EffNoop -> run handleInput handleOutput
        _ -> return ()

handleRequest CursorBack        = displayHelp
handleRequest (Load filePath)   = do
  load filePath
  printFuture
handleRequest Reload            = do
  reload
  printFuture
handleRequest Execute           = do
  reload
  execute
handleRequest Test = do
  test
handleRequest Help              = displayHelp

--------------------------------------------------------------------------------
-- | Parsing human input

parseRequest :: String -> Request
parseRequest key
  | "load" `isPrefixOf` key = (Load . trim . drop 4) key
  | "l"    `isPrefixOf` key = (Load . trim . drop 1) key
  | otherwise = case trim key of
      "\ESC[A"  -> CursorPrev
      "w"       -> CursorPrev
      "\ESC[B"  -> CursorNext
      "s"       -> CursorNext
      "\ESC[C"  -> CursorForth
      "d"       -> CursorForth
      "\ESC[D"  -> CursorBack
      "a"       -> CursorBack
      "\n"      -> CursorForth
      "h"       -> Help
      "help"    -> Help
      "r"       -> Reload
      "reload"  -> Reload
      "x"       -> Execute
      "exec"    -> Execute
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
        minput <- getInputLine "π :"
        case minput of
            Nothing     -> lift getKey
            Just input' -> return input'
    'w' -> return "w"
    'a' -> return "a"
    's' -> return "s"
    'd' -> return "d"
    _   -> interceptStdin [firstChar]

  restoreStdin
  return key

--------------------------------------------------------------------------------
-- | Printing stuff

displayHelp :: InteractionM IO ()
displayHelp = liftIO $ do
  putStrLn "========================================"
  putStrLn "  ** Pi Tracer **   "
  putStrLn "  arrow keys or 'wasd'  for navigation"
  putStrLn "  :help                 for this help message   (:h)"
  putStrLn "  :load FILEPATH        for loading files       (:l)"
  putStrLn "  :reload               for reloading           (:r)"
  putStrLn "  :exec                 for execution           (:x)"
  putStrLn "========================================"

printFuture :: InteractionM IO ()
printFuture = do
  previousState <- latestState
  next <- selectedFuture
  case next of
    Failure err -> throwError (InteractionError err)
    Success nextState EffNoop -> do
      liftIO $ do
        yellow $ putStrLn $ "\nNo-op"
      printPools [] nextState
      printStatusBar
    Success _ (EffCall caller result) -> do
      liftIO $ do
        yellow $ putStrLn $ "\nCall"
      let targets = [(getPID caller, putStrLn $ show (pretty result))]
      printPools targets previousState
      printStatusBar
    Success _ (EffReplNu replNu result) -> do
      liftIO $ do
        yellow $ putStrLn $ "\nReplicate and create new channel"
      let targets = [(getPID replNu, putStrLn $ abbreviate $ show (pretty result))]
      printPools targets previousState
      printStatusBar
    Success _ (EffIO task) -> do
      liftIO $ do
        yellow $ putStrLn $ "\nI/O"
      printPools [(getPID task, return ())] previousState
      printStatusBar
    Success _ (EffComm _ (sender, receiver) (sender', receiver')) -> do
      liftIO $ do
        yellow $ putStrLn $ "\nCommunicate"
      let printSenderResult = putStrLn $ abbreviate $ show (pretty sender')
      let printReceiverResult = putStrLn $ abbreviate $ show (pretty receiver')
      let targets = [(getPID sender, printSenderResult), (getPID receiver, printReceiverResult)]
      printPools targets previousState
      printStatusBar


abbreviate :: String -> String
abbreviate s = if length s >= 36
  then  take 32 s ++ " ..."
  else  s ++ replicate (36 - length s) ' '


printPool :: (HasPID q, Pretty q)
  => String             -- name of the "pool"
  -> [(PID, IO ())]     -- the "selected process" along with the stuff to print
  -> [q]                -- input
  -> IO ()
printPool name selected processes = do
  when (not (null processes)) $ do
    blue $ putStrLn $ "  " ++ name ++ ":" -- print the name
    forM_ processes $ \process -> do      -- for each process

      let printInvoker = green $ putStr $ "[" ++ (unpack (invokedBy process)) ++ "] "
      let printProcess = putStr $ abbreviate (show (pretty process))

      case lookup (getPID process) selected of
        Just f -> do
          red $ putStr $ " ●  "             -- print a red dot before it
          printInvoker                      -- also the process that invoked it
          printProcess                      -- print the process itself
          red $ putStr $ " => "         -- print something after a big red arrow
          f
          putStrLn ""
        Nothing -> do
          putStr $ " ○  "
          printInvoker
          printProcess
          putStrLn ""

printPools :: [(PID, IO ())] -> St -> InteractionM IO ()
printPools targets (St senders receivers callers replNus io _ _ _) = liftIO $ do
  printPool "senders"         targets (map snd senders)
  printPool "receivers"       targets (map snd receivers)
  printPool "callers"         targets callers
  printPool "replicable nus"  targets replNus
  printPool "I/O tasks"       targets io


printStatusBar :: InteractionM IO ()
printStatusBar = do
  outcomes <- gets stFuture
  cursor <- gets stCursor
  case cursor of
    Nothing -> liftIO $ putStrLn $ "0/0 outcomes"
    Just n  -> liftIO $ putStrLn $ show (n + 1) ++ "/" ++ show (length outcomes) ++ " outcomes"
