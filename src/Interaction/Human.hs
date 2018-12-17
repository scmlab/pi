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
  prompt
  loop
humanREPL True (filePath:_) = void $ runInteraction $ do
  handleError (handleRequest (Load filePath))
  prompt
  loop
humanREPL False (filePath:_) = void $ runInteraction $ do
  handleError $ do
    load filePath
    execute

prompt :: InteractionM IO ()
prompt = liftIO $ do
  putStr "π > "
  hFlush stdout

loop :: InteractionM IO ()
loop = do
  liftIO getKey >>= handleError . handleRequest . parseRequest
  prompt
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
        minput <- getInputLine ":"
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
      printEffNoop nextState
      printStatusBar
    Success _ (EffCall caller result) -> do
      liftIO $ do
        yellow $ putStrLn $ "\nCall"
      printEffCall caller result previousState
      printStatusBar
    Success _ (EffReplNu replNu result) -> do
      liftIO $ do
        yellow $ putStrLn $ "\nReplicate and create new channel"
      printEffReplNu replNu result previousState
      printStatusBar
    Success _ (EffIO task) -> do
      liftIO $ do
        yellow $ putStrLn $ "\nI/O"
      printEffIO task previousState
      printStatusBar
    Success _ (EffComm _ reagents products) -> do
      liftIO $ do
        yellow $ putStrLn $ "\nCommunicate"
      printEffComm reagents products previousState
      printStatusBar

abbreviate :: String -> String
abbreviate s = if length s >= 36
  then  take 32 s ++ " ..."
  else  s ++ replicate (36 - length s) ' '

printSenders :: (Sender -> Bool) -> IO () -> [(Name, Sender)] -> IO ()
printSenders p printer senders = do
  when (not $ null senders) $ do
    blue $ putStrLn $ "  senders:"
    mapM_ printSender senders
  where
    printSender :: (Name, Sender) -> IO ()
    printSender (_, sender) = do
      if p sender
        then do
          red $ putStr $ " ●  "
          green $ putStr $ "[" ++ (unpack $ invokedBy sender) ++ "] "
          putStr $ abbreviate (show (pretty sender))
          red $ putStr $ " => "
          printer
        else do
          putStr $ " ○  "
          green $ putStr $ "[" ++ (unpack $ invokedBy sender) ++ "] "
          putStrLn $ abbreviate (show (pretty sender))


printReceivers :: (Receiver -> Bool) -> IO () -> [(Name, Receiver)] -> IO ()
printReceivers p printer receivers = do
  when (not $ null receivers) $ do
    blue $ putStrLn $ "  receivers:"
    forM_ receivers $ \(_, receiver) -> do
      if p receiver
        then do
          red $ putStr $ " ●  "
          green $ putStr $ "[" ++ (unpack $ invokedBy receiver) ++ "] "
          putStr $ abbreviate (show (pretty receiver))
          red $ putStr $ " => "
          printer
        else do
          putStr $ " ○  "
          green $ putStr $ "[" ++ (unpack $ invokedBy receiver) ++ "] "
          putStrLn $ abbreviate (show (pretty receiver))

printCallers :: (Caller -> Bool) -> IO () -> [Caller] -> IO ()
printCallers p printer callers = do
  when (not $ null callers) $ do
    blue $ putStrLn $ "  callers:"
    forM_ callers $ \caller -> do
      if p caller
        then do
          red $ putStr $ " ●  "
          green $ putStr $ "[" ++ (unpack $ invokedBy caller) ++ "] "
          putStr $ abbreviate (show (pretty caller))
          red $ putStr $ " => "
          printer
        else do
          putStr $ " ○  "
          green $ putStr $ "[" ++ (unpack $ invokedBy caller) ++ "] "
          putStrLn $ abbreviate (show (pretty caller))

printReplNus :: (ReplNu -> Bool) -> IO () -> [ReplNu] -> IO ()
printReplNus p printer replNus = do
  when (not $ null replNus) $ do
    blue $ putStrLn $ "  replicable nus:"
    forM_ replNus $ \replNu -> do
      if p replNu
        then do
          red $ putStr $ " ●  "
          green $ putStr $ "[" ++ (unpack $ invokedBy replNu) ++ "] "
          putStr $ abbreviate (show (pretty replNu))
          red $ putStr $ " => "
          printer
        else do
          putStr $ " ○  "
          green $ putStr $ "[" ++ (unpack $ invokedBy replNu) ++ "] "
          putStrLn $ abbreviate (show (pretty replNu))


printIOTasks :: (IOTask -> Bool) -> [IOTask] -> IO ()
printIOTasks p tasks = do
  when (not $ null tasks) $ do
    blue $ putStrLn $ "  io:"
    forM_ tasks $ \task -> do
      if p task
        then red $ putStr $ " ●  "
        else putStr $ " ○  "
      green $ putStr $ "[" ++ (unpack $ invokedBy task) ++ "] "
      putStrLn $ abbreviate (show (pretty task))

printEffCall :: Caller -> Pi -> St -> InteractionM IO ()
printEffCall selected result (St senders receivers callers replNus io _ _ _) = do
  liftIO $ do
    printSenders   (const False) (return ()) senders
    printReceivers (const False) (return ()) receivers
    printCallers   ((==) selected) (putStrLn $ show (pretty result)) callers
    printReplNus   (const False) (return ()) replNus
    printIOTasks   (const False) io

printEffReplNu :: ReplNu -> Pi -> St -> InteractionM IO ()
printEffReplNu selected result (St senders receivers callers replNus io _ _ _) = do
  liftIO $ do
    printSenders   (const False) (return ()) senders
    printReceivers (const False) (return ()) receivers
    printCallers   (const False) (return ()) callers
    printReplNus   ((==) selected) (putStrLn $ abbreviate $ show (pretty result)) replNus
    printIOTasks   (const False) io

printEffComm :: (Sender, Receiver) -> (Pi, Pi) -> St -> InteractionM IO ()
printEffComm (selectedSender, selectedReceiver) (productSender, productReceiver) (St senders receivers callers replNus io _ _ _) = do
  liftIO $ do
    printSenders   ((==) selectedSender)   (putStrLn $ abbreviate $ show (pretty productSender)) senders
    printReceivers ((==) selectedReceiver) (putStrLn $ abbreviate $ show (pretty productReceiver)) receivers
    printCallers   (const False) (return ()) callers
    printReplNus   (const False) (return ()) replNus
    printIOTasks   (const False) io

printEffIO :: IOTask -> St -> InteractionM IO ()
printEffIO selected@(Input _ _) (St senders receivers callers replNus io _ _ _) = liftIO $ do
    printSenders   (const False) (return ()) senders
    printReceivers (const False) (return ()) receivers
    printCallers   (const False) (return ()) callers
    printReplNus   (const False) (return ()) replNus
    printIOTasks   ((==) selected) io
printEffIO selected@(Output _ _ _) (St senders receivers callers replNus io _ _ _) = liftIO $ do
    printSenders   (const False) (return ()) senders
    printReceivers (const False) (return ()) receivers
    printCallers   (const False) (return ()) callers
    printReplNus   (const False) (return ()) replNus
    printIOTasks   ((==) selected) io

printEffNoop :: St -> InteractionM IO ()
printEffNoop (St senders receivers callers replNus io _ _ _) = do
  liftIO $ do
    printSenders   (const False) (return ()) senders
    printReceivers (const False) (return ()) receivers
    printCallers   (const False) (return ()) callers
    printReplNus   (const False) (return ()) replNus
    printIOTasks   (const False) io

printStatusBar :: InteractionM IO ()
printStatusBar = do
  outcomes <- gets stFuture
  cursor <- gets stCursor
  case cursor of
    Nothing -> liftIO $ putStrLn $ "0/0 outcomes"
    Just n  -> liftIO $ putStrLn $ show (n + 1) ++ "/" ++ show (length outcomes) ++ " outcomes"
