{-# LANGUAGE OverloadedStrings #-}

module Runtime where

import Control.Monad.State hiding (State, state)
import Control.Monad.Except
import qualified Control.Exception as Exception
import qualified Data.Map as Map
-- import Data.Map (Map)
import Control.Exception (IOException)
import Data.Text.Prettyprint.Doc
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (mapMaybe)

import Syntax.Abstract
import Type.TypeCheck
-- import Syntax.Concrete (restore)
import qualified Syntax.Parser as Parser
import Interpreter


 --------------------------------------------------------------------------------
 -- | Runtime Monad

data Outcome = Success St Effect
             | Failure ErrMsg
             deriving (Show)

instance Pretty Outcome where
  pretty (Failure msg)          = pretty ("error:" :: String) <+> pretty msg
  pretty (Success _ reaction) = pretty reaction

data RuntimeState = State
  { stFilePath :: Maybe String        -- loaded filepath
  , stSource   :: Maybe ByteString    -- source code
  , stEnv      :: Maybe Env           -- syntax tree
  , stHistory  :: [Outcome]           -- history outcomes
  , stFuture   :: [Outcome]           -- next possible outcomes
  , stCursor   :: Maybe Int           -- pointing at which outcome
  } deriving (Show)

data Error = ParseError Parser.ParseError
           | TypeError TypeError
           | RuntimeError String
           deriving (Show)

type RuntimeM = ExceptT Error (StateT RuntimeState IO)

--------------------------------------------------------------------------------

putFilePath :: Maybe FilePath -> RuntimeM ()
putFilePath x = modify $ \ st -> st { stFilePath = x }

putSource :: Maybe ByteString -> RuntimeM ()
putSource x = modify $ \ st -> st { stSource = x }

putEnv :: Maybe Env -> RuntimeM ()
putEnv x = modify $ \ st -> st { stEnv = x }

pushHistory :: Outcome -> RuntimeM ()
pushHistory x = do
  xs <- gets stHistory
  modify $ \st -> st { stHistory = x:xs }

updateFuture :: [Outcome] -> RuntimeM ()
updateFuture outcomes = modify $ \ st -> st
  { stFuture = outcomes
  , stCursor = if null outcomes then Nothing else Just 0
  }

putCursor :: Maybe Int -> RuntimeM ()
putCursor x = modify $ \ st -> st { stCursor = x }

--------------------------------------------------------------------------------

runRuntimeM :: RuntimeM a -> IO (Either Error a, RuntimeState)
runRuntimeM handler =
  runStateT (runExceptT handler) (State Nothing Nothing Nothing [] initialOutcomes Nothing)
  where initialOutcomes = [Failure "please load first"]

interpret :: Env -> St -> PiMonad Effect -> [Outcome]
interpret env st program = map toOutcome (runPiMonad env st program)
  where
    toOutcome :: Either String (Effect, St) -> Outcome
    toOutcome (Left err)              = Failure err
    toOutcome (Right (effect, state)) = Success state effect


--------------------------------------------------------------------------------
-- | Cursor related operations

withCursor :: (Int -> RuntimeM a) -> RuntimeM a
-- withCursor :: (Int -> RuntimeM a) -> RuntimeM a
withCursor f = do
  cursor <- gets stCursor
  case cursor of
    Nothing -> throwError $ RuntimeError "cannot go any further"
    Just n -> f n

choose :: Int -> RuntimeM ()
choose n = do
  len <- length <$> gets stFuture
  if n >= len then
    putCursor (Just 0)
  else if n < 0 then
    putCursor (Just (len - 1))
  else do
    putCursor (Just n)

-- retrieve the next appointed outcome
selectedFuture :: RuntimeM Outcome
selectedFuture = do
  withCursor $ \n -> (!! n) <$> gets stFuture

latestHistory :: RuntimeM Outcome
latestHistory = do
  history <- gets stHistory
  if null history
    then throwError $ RuntimeError "no history to retrieve from"
    else return (head history)

latestState :: RuntimeM St
latestState = do
  outcome <- latestHistory
  case outcome of
    (Success state _) -> return state
    _ -> throwError $ RuntimeError "cannot retrieve state"

--------------------------------------------------------------------------------
-- | Commands

load :: FilePath -> RuntimeM ()
load filePath = do
  -- storing the filepath
  putFilePath (Just filePath)
  -- storing the source
  readResult <- liftIO $ Exception.try (BS.readFile filePath)
  case readResult of
    Left  err -> throwError $ RuntimeError $ show (err :: IOException)
    Right source -> do
      putSource (Just source)
      -- parse and store the AST
      case Parser.parseProgram filePath source of
        Left err  -> throwError $ ParseError err
        Right ast -> programToEnv ast >>= putEnv . Just


      -- do some checkings
      env' <- gets stEnv
      case env' of
        Just defns -> do
          let result = runTCM checkAll defns
          case result of
            Left err -> throwError $ TypeError err
            Right _ -> return ()
        Nothing -> return ()


      env <- getEnv
      -- populate future, the next possible outcomes (there should be only 1)

      -- run "test" or else run "main"
      let procToRun = if Map.member "test" (envProcDefns env)
                        then "test" else "main"
      updateFuture $ interpret env initialState $ do
        _ <- call (Caller (PID False "you" (-1)) procToRun)
        return EffNoop


      -- retrieve state from the recently populated outcome and store it
      outcome <- selectedFuture
      case outcome of
        Success state reaction -> do
          pushHistory (Success state reaction)
          updateFuture $ interpret env state step
        Failure _ -> throwError $ RuntimeError "cannot retrieve outcome"

test :: RuntimeM ()
test = do
  filePath <- getFilePath
  rawFile <- liftIO $ BS.readFile filePath
  case Parser.parseByteString2 filePath rawFile of
    Left err   -> error $ show err
    Right _  -> do
      latestState >>= liftIO . print


-- read and parse and store program from the stored filepath
reload :: RuntimeM ()
reload = do
  filePath <- getFilePath
  load filePath

-- run the appointed outcome
run :: RuntimeM Val   -- input handler
  -> (Val -> RuntimeM ())   -- output handler
  -> RuntimeM ()
run inputHandler outputHandler = do
  outcome <- selectedFuture
  pushHistory outcome
  case outcome of
    Failure err -> do
      updateFuture $ [Failure err]
    Success oldState (EffCall _ _) -> do
      env <- getEnv
      updateFuture $ interpret env oldState step
    Success oldState (EffReplNu _ _) -> do
      env <- getEnv
      updateFuture $ interpret env oldState step
    Success oldState (EffComm _ _ _) -> do
      env <- getEnv
      updateFuture $ interpret env oldState step
    -- Output
    Success oldState (EffIO (Output pid val p)) -> do
      outputHandler val
      env <- getEnv
      updateFuture $ interpret env oldState $ do
        lineup [(pInvokedBy pid, p)]
        return EffNoop
    -- Input
    Success oldState (EffIO task) -> do
      val <- inputHandler
      env <- getEnv
      updateFuture $ interpret env oldState $ do
        input val task
        return EffNoop
    Success oldState EffNoop -> do
      env <- getEnv
      updateFuture $ interpret env oldState step

--------------------------------------------------------------------------------
-- | Helpers

-- get existing filepath from the state
getFilePath :: RuntimeM FilePath
getFilePath = do
  result <- gets stFilePath
  case result of
    Nothing       -> throwError $ RuntimeError "please load the program first"
    Just filePath -> return filePath

getEnv :: RuntimeM Env
getEnv = do
  result <- gets stEnv
  case result of
    Nothing   -> throwError $ RuntimeError "panic: the AST has not been parsed and stored"
    Just env  -> return env

--------------------------------------------------------------------------------
-- | Request

data Request
  = CursorMoveTo Int
  | CursorNext | CursorPrev
  | CursorBack | CursorForth
  | Help
  | Test
  | Load String
  | Reload
  | Execute
  deriving (Show)

--------------------------------------------------------------------------------
-- | Converting parsed program

programToEnv :: Program -> RuntimeM Env
programToEnv (Program declarations) = do
  -- -- throw if there is any type signature without a corresponding process definition
  -- unless (Map.null onlyTypes) $
  --   throwError $ TypeError $ MissingProcDefn onlyTypes
  chanTypes' <- Map.fromList <$> forM chanTypes (\(n, t) -> toSName n >>= \n' -> return (n', t) )
  return $ Env chanTypes' procDefns
  where
    toChanTypePair (ChanType n t) = Just (n, t)
    toChanTypePair _              = Nothing

    toProcDefnPair (ProcDefn n t) = Just (n, t)
    toProcDefnPair _              = Nothing

    chanTypes = mapMaybe toChanTypePair declarations
    procDefns = Map.fromList $ mapMaybe toProcDefnPair declarations

toSName :: Name -> RuntimeM SName
toSName (ND c) = return c
toSName n@(NG _) = throwError $ TypeError $ SNameExpected n
toSName n@(NR _) = throwError $ TypeError $ SNameExpected n
