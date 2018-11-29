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
  previous <- latestHistory
  selectedFuture >>= printOutcome (Just previous)
handleRequest CursorPrev          = withCursor $ \n -> try $ do
  choose (n - 1)
  previous <- latestHistory
  selectedFuture >>= printOutcome (Just previous)
handleRequest CursorNext        = withCursor $ \n -> try $ do
  choose (n + 1)
  previous <- latestHistory
  selectedFuture >>= printOutcome (Just previous)
handleRequest CursorForth        = do
  run
  previous <- latestHistory
  selectedFuture >>= printOutcome (Just previous)
handleRequest CursorBack        = displayHelp
  -- run
  -- previous <- latestHistory
  -- selectedFuture >>= printOutcome (Just previous)
  --
handleRequest (Load filePath)   = do
  load filePath
  previous <- latestHistory
  selectedFuture >>= printOutcome (Just previous)
handleRequest Reload            = do
  reload
  previous <- latestHistory
  selectedFuture >>= printOutcome (Just previous)
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
      "\ESC[B"  -> CursorNext
      "\ESC[C"  -> CursorForth
      "\ESC[D"  -> CursorBack
      "\n"      -> CursorForth
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
            Just input' -> return input'
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


--------------------------------------------------------------------------------
-- | Printing stuff

displayHelp :: InteractionM IO ()
displayHelp = liftIO $ do
  putStrLn "========================================"
  putStrLn "  arrow keys          for navigation"
  putStrLn "  :help               for this help message   (:h)"
  putStrLn "  :load FILEPATH      for loading files       (:l)"
  putStrLn "  :reload             for reloading           (:r)"
  putStrLn "========================================"

printOutcome :: Maybe Outcome -> Outcome -> InteractionM IO ()
printOutcome previous next = case next of
    Failure err -> throwError (InteractionError err)
    Success nextState Silent _ -> do
      liftIO $ do
        setSGR [SetColor Foreground Vivid Yellow]
        putStr $ "\nNo-op"
        setSGR []
      printState nextState
      printStatusBar
    Success nextState (Output sender) _ -> do
      liftIO $ do
        setSGR [SetColor Foreground Vivid Yellow]
        putStrLn $ "\nOutput "
        setSGR []

      case previous of
        Just (Success previousState _ _) -> printOutput sender previousState
        _ -> throwError $ InteractionError "panic: cannot retrieve the state before the output"

      printStatusBar
    Success _ (React _ reagents products) _ -> do
      liftIO $ do
        setSGR [SetColor Foreground Vivid Yellow]
        putStrLn $ "\nReact"
        setSGR []

      case previous of
        Just (Success previousState _ _) -> printReact reagents products previousState
        _ -> throwError $ InteractionError "panic: cannot retrieve the state before the reaction"

      printStatusBar
    Success nextState (Input _) _ -> do
      printState nextState
      printStatusBar

blue :: IO () -> IO ()
blue p = do
  setSGR [SetColor Foreground Dull Blue]
  p
  setSGR []

red :: IO () -> IO ()
red p = do
  setSGR [SetColor Foreground Vivid Red]
  p
  setSGR []

green :: IO () -> IO ()
green p = do
  setSGR [SetColor Foreground Vivid Green]
  p
  setSGR []

abbreviate :: String -> String
abbreviate s = if length s >= 38
  then  take 34 s ++ " ~~~"
  else  s ++ replicate (38 - length s) ' '

-- printReceiver :: Receiver -> InteractionM IO ()
-- printReceiver ()

printOutput :: Sender -> St -> InteractionM IO ()
printOutput selected@(Sender i v _) (St senders receivers waitings _ _) = do
  let ss = [ (selected == sender, senderToPi c sender) | (c, sender)          <- senders   ]
  let rs = [ receiverToPi c receiver        | (c, receiver)        <- receivers ]
  let is = [ Recv (NR StdIn) clauses       | (Receiver _ clauses) <- waitings  ]

  liftIO $ do
    blue $ putStrLn $ "  senders:"
    when (not $ null ss) $ do
      forM_ ss $ \(selected, p) -> do
        if selected
          then do
            red $ putStr $ "☞   "
            putStr $ abbreviate (show (pretty p))
            red $ putStr $ " => "
            green $ putStrLn $ show $ pretty v
          else putStrLn $ show $ indent 4 (pretty p)

    when (not $ null rs) $ do
      blue $ putStrLn $ "  receivers:"
      putStrLn $ show $ indent 4 (vsep (map pretty rs))

    when (not $ null is) $ do
      blue $ putStrLn $ "  blocked:"
      putStrLn $ show $ indent 4 (vsep (map pretty is))

printReact :: (Sender, Receiver) -> (Pi, Pi) -> St -> InteractionM IO ()
printReact (sender, receiver) (sender', receiver') (St senders receivers waitings _ _) = do
  let ss = [ (sender   == selected,   senderToPi c selected) | (c, selected)         <- senders   ]
  let rs = [ (receiver == selected, receiverToPi c selected) | (c, selected) <- receivers ]
  let is = [ Recv (NR StdIn) clauses                         | (Receiver _ clauses)      <- waitings  ]

  liftIO $ do
    blue $ putStrLn $ "  senders:"
    when (not $ null ss) $ do
      forM_ ss $ \(selected, p) -> do
        if selected
          then do
            red $ putStr $ "☞   "
            putStr $ abbreviate (show (pretty p))
            red $ putStr $ " => "
            putStrLn $ abbreviate $ show (pretty sender')
          else putStrLn $ show $ indent 4 (pretty p)

    when (not $ null rs) $ do
      blue $ putStrLn $ "  receivers:"
      forM_ rs $ \(selected, p) -> do
        if selected
          then do
            red $ putStr $ "☞   "
            putStr $ abbreviate (show (pretty p))
            red $ putStr $ " => "
            putStrLn $ abbreviate $ show (pretty receiver')
          else putStrLn $ show $ indent 4 (pretty p)

    when (not $ null is) $ do
      blue $ putStrLn $ "  blocked:"
      putStrLn $ show $ indent 4 (vsep (map pretty is))


printState :: St -> InteractionM IO ()
printState (St senders receivers waitings _ _) = do

  let ss = [ senderToPi   c selected  | (c, selected)        <- senders   ]
  let rs = [ receiverToPi c selected  | (c, selected)        <- receivers ]
  let is = [ Recv (NR StdIn) clauses | (Receiver _ clauses) <- waitings  ]

  liftIO $ do
    when (not $ null ss) $ do
      blue $ putStrLn $ "  senders:"
      putStrLn $ show $ indent 4 (vsep (map pretty ss))

    when (not $ null rs) $ do
      blue $ putStrLn $ "  receivers:"
      putStrLn $ show $ indent 4 (vsep (map pretty rs))

    when (not $ null is) $ do
      blue $ putStrLn $ "  blocked:"
      putStrLn $ show $ indent 4 (vsep (map pretty is))

printStatusBar :: InteractionM IO ()
printStatusBar = do
  outcomes <- gets stFuture
  cursor <- gets stCursor
  case cursor of
    Nothing -> liftIO $ putStrLn $ "0/0 outcomes"
    Just n  -> liftIO $ putStrLn $ show (n + 1) ++ "/" ++ show (length outcomes) ++ " outcomes"
