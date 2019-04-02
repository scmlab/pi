{-# LANGUAGE OverloadedStrings #-}

module Runtime.Human where

import Control.Monad.State hiding (State, state)
import Control.Monad.Except
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import Data.Text (Text)
import Data.Loc (Loc(..), Located(..))
import Data.ByteString.Lazy (ByteString)

import qualified Data.Loc as Loc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text.Prettyprint.Doc
import Prelude hiding (readFile)
import System.Console.Haskeline
import System.IO
import Text.Read (readMaybe)

import Pretty
import Runtime
import Runtime.Scheduler
import Runtime.Util
-- import Interpreter (St(..))
import Interpreter
import Syntax.Abstract
import Type.TypeCheck (TypeError(..))
import Syntax.Parser.Type

--------------------------------------------------------------------------------
-- | Interfacing with Humans

humanREPtrnLabel :: Bool -> [FilePath] -> IO ()
humanREPtrnLabel _traceMode [] = void $ runRuntimeM $ do
  displayHelp
  loop
humanREPtrnLabel True (filePath:_) = void $ runRuntimeM $ do
  handleError (handleRequest (Load filePath))
  loop
humanREPtrnLabel False (filePath:_) = void $ runRuntimeM $ do
  handleError $ do
    load filePath
    execute

loop :: RuntimeM ()
loop = do
  liftIO getKey >>= handleError . handleRequest . parseRequest
  loop

handleError :: RuntimeM () -> RuntimeM ()
handleError program = program `catchError` \ err -> case err of
  ParseError parseError -> prettyParseError parseError >>= liftIO . putDoc
  TypeError typeError -> prettyTypeError typeError >>= liftIO . putDoc
  RuntimeError msg -> liftIO (putStrLn (show msg))

try :: RuntimeM () -> RuntimeM ()
try program = program `catchError` \_ -> return ()

handleRequest :: Request -> RuntimeM ()
handleRequest (CursorMoveTo n)  = do
  choose n
  printFuture
handleRequest CursorPrev        = withCursor $ \n -> do
  choose (n - 1)
  printFuture
handleRequest CursorNext        = withCursor $ \n -> do
  choose (n + 1)
  printFuture
handleRequest CursorForth        = do
  run handleInput handleOutput
  skipSilent
  printFuture
  where
    handleInput :: RuntimeM Val
    handleInput = do
      raw <- liftIO $ do
        yellow $ text $ "\nInput:"
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
          liftIO $ red $ text $ "cannot parse the input"
          handleInput

    handleOutput :: Val -> RuntimeM ()
    handleOutput val = liftIO $ do
      yellow $ text $ "\nOutput:"
      green val

    skipSilent :: RuntimeM ()
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

displayHelp :: RuntimeM ()
displayHelp = liftIO $ do
  putStrLn "========================================"
  putStrLn "  ** Pi Tracer **   "
  putStrLn "  arrow keys or 'wasd'  for navigation"
  putStrLn "  :help                 for this help message   (:h)"
  putStrLn "  :load FILEPATH        for loading files       (:l)"
  putStrLn "  :reload               for reloading           (:r)"
  putStrLn "  :exec                 for execution           (:x)"
  putStrLn "========================================"

printFuture :: RuntimeM ()
printFuture = do
  previousState <- latestState
  next <- selectedFuture
  case next of
    Failure err -> throwError (RuntimeError err)
    Success nextState EffNoop -> do
      liftIO $ do
        yellow $ text $ "\nNo-op"
      printPools [] nextState
      printStatusBar
    Success _ (EffCall caller result) -> do
      liftIO $ do
        yellow $ text $ "\nCall"
      let targets = [(getPID caller, putStr $ show (pretty result))]
      printPools targets previousState
      printStatusBar
    Success _ (EffReplNu replNu result) -> do
      liftIO $ do
        yellow $ text $ "\nReplicate and create new channel"
      let targets = [(getPID replNu, putStr $ abbreviate $ show (pretty result))]
      printPools targets previousState
      printStatusBar
    Success _ (EffIO task) -> do
      liftIO $ do
        yellow $ text $ "\nI/O"
      printPools [(getPID task, return ())] previousState
      printStatusBar
    Success _ (EffComm _ (sender, receiver) (sender', receiver')) -> do
      liftIO $ do
        yellow $ text $ "\nCommunicate"
      let printSenderResult = putStr $ abbreviate $ show (pretty sender')
      let printReceiverResult = putStr $ abbreviate $ show (pretty receiver')
      let targets = [(getPID sender, printSenderResult), (getPID receiver, printReceiverResult)]
      printPools targets previousState
      printStatusBar


abbreviate :: String -> String
abbreviate s = if length s >= 36
  then  take 32 s ++ " ..."
  else  s ++ replicate (36 - length s) ' '


printPool :: (HasPID q, Pretty q)
  => Text               -- name of the "pool"
  -> [(PID, IO ())]     -- the "selected process" along with the stuff to print
  -> [q]                -- input
  -> IO ()
printPool name selected processes = do
  when (not (null processes)) $ do
    blue $ text $ "  " <> name <> ":" -- print the name
    forM_ processes $ \process -> do      -- for each process

      let printInvoker = green $ text $ "[" <> invokedBy process <> "] "
      let printProcess = putStr $ abbreviate (show (pretty process))

      case lookup (getPID process) selected of
        Just f -> do
          red $ text " ●  "             -- print a red dot before it
          printInvoker                      -- also the process that invoked it
          printProcess                      -- print the process itself
          red $ text " => "          -- print something after a big red arrow
          f
          putStrLn ""
        Nothing -> do
          putStr $ " ○  "
          printInvoker
          printProcess
          putStrLn ""

printPools :: [(PID, IO ())] -> St -> RuntimeM ()
printPools targets (St senders receivers callers replNus io _ _ _) = liftIO $ do
  printPool "senders"         targets (map snd senders)
  printPool "receivers"       targets (map snd receivers)
  printPool "callers"         targets callers
  printPool "replicable nus"  targets replNus
  printPool "I/O tasks"       targets io


printStatusBar :: RuntimeM ()
printStatusBar = do
  outcomes <- gets stFuture
  cursor <- gets stCursor
  case cursor of
    Nothing -> liftIO $ putStrLn $ "0/0 outcomes"
    Just n  -> liftIO $ putStrLn $ show (n + 1) ++ "/" ++ show (length outcomes) ++ " outcomes"


--------------------------------------------------------------------------------
-- | Printing errors

getSourceOrThrow :: RuntimeM ByteString
getSourceOrThrow = do
  result <- gets stSource
  case result of
    Nothing -> throwError $ RuntimeError "Can't read the stored source code"
    Just source -> return source

prettyError :: Text -> [Doc AnsiStyle] -> [Loc] -> RuntimeM (Doc AnsiStyle)
prettyError header paragraphs locations = do
  let message = vsep $
        [ annotate (color Red) $ pretty header
        , softline
        ]
        ++ (map (\ x -> x <> line) paragraphs)
  source <- getSourceOrThrow
  let pieces = vsep $ map (\loc -> vsep
        [ annotate (colorDull Blue) (pretty $ Loc.displayLoc loc)
        , reAnnotate toAnsiStyle $ prettySourceCode $ SourceCode source loc 1
        ]) locations

  return $ vsep
    [ softline'
    , indent 2 message
    , indent 2 pieces
    , softline'
    ]

prettyParseError :: ParseError -> RuntimeM (Doc AnsiStyle)
prettyParseError (Lexical pos) = do
  prettyError "Lexical parse error" []
    [locOf pos]
prettyParseError (Syntatical loc _) = do
  prettyError "Lexical parse error" []
    [loc]

prettyTypeError :: TypeError -> RuntimeM (Doc AnsiStyle)
prettyTypeError e = case e of


  TypeMismatched expected actual -> prettyError
    "Type Mismatch"
    (message ++
    [   "when checking the following term"
    ]) [locOf actual]
    where message =
            [      "expected: " <> highlight expected <> line
                <> "  actual: " <> highlight actual
            ]
  Others s -> prettyError "Others" [pretty s] []
  others -> prettyError "" [pretty $ show others] []
--  MissingProcDefn (Map ProcName Type)
-- | VariableNotFound Chan
-- | TypeVariableNotFound TypeVar
-- | TypeVarIndexAtTopLevel TypeVar
-- | LabelNotFound Label
-- | ProcessNotFound ProcName
-- | PatternMismatched Type Ptrn
-- | TypeOfNewChannelMissing Chan
-- | RecvExpected Type
-- | SendExpected Type
-- | UserDefinedNameExpected Chan
-- | Others String


-- prettyTypeError (TypeSigDuplicated a b) =
--   prettyError "Duplicating type signature" Nothing
--     [locOf a, locOf b]
-- prettyTypeError (TermDefnDuplicated a b) =
--   prettyError "Duplicating term definition" Nothing
--     [locOf a, locOf b]
-- -- prettyTypeError (TypeSigNotFound a) =
-- --   prettyError "Missing type signature" (Just "the following term has no type signature")
-- --     [locOf a]
-- -- prettyTypeError (TermDefnNotFound a) =
-- --   prettyError "Missing term definition" (Just "the following type has no term definition")
-- --     [locOf a]
-- prettyTypeError (InferError a) =
--    prettyInferError a
-- prettyTypeError (Others msg) =
--   prettyError "Other unformatted type errors" (Just msg) []
--
highlight :: Pretty a => a -> Doc AnsiStyle
highlight = annotate (colorDull Blue) . pretty
