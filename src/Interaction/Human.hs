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
        Success _ Silent _ -> run handleInput handleOutput
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
        minput <- getInputLine "π : "
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
    Success nextState Silent _ -> do
      liftIO $ do
        yellow $ putStrLn $ "\nNo-op"
      printState nextState
      printStatusBar
    Success _ (Output sender) _ -> do
      liftIO $ do
        yellow $ putStrLn $ "\nOutput"
      printOutput sender previousState
      printStatusBar
    Success _ (React _ reagents products) _ -> do
      liftIO $ do
        yellow $ putStrLn $ "\nReact"
      printReact reagents products previousState
      printStatusBar
    Success _ (Input receiver) _ -> do
      liftIO $ do
        yellow $ putStrLn $ "\nInput"
      printInput receiver previousState
      printStatusBar

abbreviate :: String -> String
abbreviate s = if length s >= 36
  then  take 32 s ++ " ..."
  else  s ++ replicate (36 - length s) ' '

printSenders :: (Sender -> Bool) -> IO () -> [(Name, Sender)] -> IO ()
printSenders p printer senders = do
  when (not $ null senders) $ do
    blue $ putStrLn $ "  senders:"
    forM_ senders $ \(c, sender) -> do
      if p sender
        then do
          red $ putStr $ " ●  "
          green $ putStr $ "[" ++ (unpack $ senderProcName sender) ++ "] "
          putStr $ abbreviate (show (pretty (senderToPi (c, sender))))
          red $ putStr $ " => "
          printer
        else do
          putStr $ " ○  "
          green $ putStr $ "[" ++ (unpack $ senderProcName sender) ++ "] "
          putStrLn $ abbreviate (show (pretty (senderToPi (c, sender))))

printReceivers :: (Receiver -> Bool) -> IO () -> [(Name, Receiver)] -> IO ()
printReceivers p printer receivers = do
  when (not $ null receivers) $ do
    blue $ putStrLn $ "  receivers:"
    forM_ receivers $ \(c, receiver) -> do
      if p receiver
        then do
          red $ putStr $ " ●  "
          green $ putStr $ "[" ++ (unpack $ receiverProcName receiver) ++ "] "
          putStr $ abbreviate (show (pretty (receiverToPi (c, receiver))))
          red $ putStr $ " => "
          printer
        else do
          putStr $ " ○  "
          green $ putStr $ "[" ++ (unpack $ receiverProcName receiver) ++ "] "
          putStrLn $ abbreviate (show (pretty (receiverToPi (c, receiver))))


printBlocked :: (Receiver -> Bool) -> IO () -> [Receiver] -> IO ()
printBlocked p printer receivers = do
  when (not $ null receivers) $ do
    blue $ putStrLn $ "  blocked:"
    forM_ receivers $ \receiver -> do
      if p receiver
        then do
          red $ putStr $ " ●  "
          green $ putStr $ "[" ++ (unpack $ receiverProcName receiver) ++ "] "
          putStrLn $ abbreviate (show (pretty (inputToPi receiver)))
          printer
        else do
          putStr $ " ○  "
          green $ putStr $ "[" ++ (unpack $ receiverProcName receiver) ++ "] "
          putStrLn $ abbreviate (show (pretty (inputToPi receiver)))

printOutput :: Sender -> St -> InteractionM IO ()
printOutput selected@(Sender _ v _) (St senders receivers blocked _ _) = do
  liftIO $ do
    printSenders ((==) selected) (green $ putStrLn $ show $ pretty v) senders
    printReceivers (const False) (return ()) receivers
    printBlocked   (const False) (return ()) blocked

printReact :: (Sender, Receiver) -> (Pi, Pi) -> St -> InteractionM IO ()
printReact (selectedSender, selectedReceiver) (productSender, productReceiver) (St senders receivers blocked _ _) = do
  liftIO $ do
    printSenders   ((==) selectedSender)   (putStrLn $ abbreviate $ show (pretty productSender)) senders
    printReceivers ((==) selectedReceiver) (putStrLn $ abbreviate $ show (pretty productReceiver)) receivers
    printBlocked   (const False) (return ()) blocked

printInput :: Receiver -> St -> InteractionM IO ()
printInput selected (St senders receivers blocked _ _) = do
  liftIO $ do
    printSenders   (const False) (return ()) senders
    printReceivers (const False) (return ()) receivers
    printBlocked   ((==) selected) (return ()) blocked
  -- where
  --   readInput :: IO (V)
  --   readInput = do
  --     hFlush stdout
  --     restoreStdin
  --     input <- getLine
  --     controlStdin
  --     return input

printState :: St -> InteractionM IO ()
printState (St senders receivers blocked _ _) = do
  liftIO $ do
    printSenders   (const False) (return ()) senders
    printReceivers (const False) (return ()) receivers
    printBlocked   (const False) (return ()) blocked

printStatusBar :: InteractionM IO ()
printStatusBar = do
  outcomes <- gets stFuture
  cursor <- gets stCursor
  case cursor of
    Nothing -> liftIO $ putStrLn $ "0/0 outcomes"
    Just n  -> liftIO $ putStrLn $ show (n + 1) ++ "/" ++ show (length outcomes) ++ " outcomes"
