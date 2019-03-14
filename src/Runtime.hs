{-# LANGUAGE OverloadedStrings #-}

module Runtime where

import Control.Monad.State hiding (State, state)
import Control.Monad.Except
import qualified Control.Exception as Exception
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Exception (IOException)
import Data.Text.Prettyprint.Doc
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (mapMaybe)

import Syntax.Abstract
import Type
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

type RuntimeM m = ExceptT Error (StateT RuntimeState m)

--------------------------------------------------------------------------------

putFilePath :: Monad m => Maybe FilePath -> RuntimeM m ()
putFilePath x = modify $ \ st -> st { stFilePath = x }

putSource :: Monad m => Maybe ByteString -> RuntimeM m ()
putSource x = modify $ \ st -> st { stSource = x }

putEnv :: Monad m => Maybe Env -> RuntimeM m ()
putEnv x = modify $ \ st -> st { stEnv = x }

pushHistory :: Monad m => Outcome -> RuntimeM m ()
pushHistory x = do
  xs <- gets stHistory
  modify $ \st -> st { stHistory = x:xs }

updateFuture :: Monad m => [Outcome] -> RuntimeM m ()
updateFuture outcomes = modify $ \ st -> st
  { stFuture = outcomes
  , stCursor = if null outcomes then Nothing else Just 0
  }

putCursor :: Monad m => Maybe Int -> RuntimeM m ()
putCursor x = modify $ \ st -> st { stCursor = x }

--------------------------------------------------------------------------------

runRuntimeM :: Monad m => RuntimeM m a -> m (Either Error a, RuntimeState)
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

withCursor :: Monad m => (Int -> RuntimeM m a) -> RuntimeM m a
-- withCursor :: Monad m => (Int -> RuntimeM m a) -> RuntimeM m a
withCursor f = do
  cursor <- gets stCursor
  case cursor of
    Nothing -> throwError $ RuntimeError "cannot go any further"
    Just n -> f n

choose :: Monad m => Int -> RuntimeM m ()
choose n = do
  len <- length <$> gets stFuture
  if n >= len then
    putCursor (Just 0)
  else if n < 0 then
    putCursor (Just (len - 1))
  else do
    putCursor (Just n)

-- retrieve the next appointed outcome
selectedFuture :: Monad m => RuntimeM m Outcome
selectedFuture = do
  withCursor $ \n -> (!! n) <$> gets stFuture

latestHistory :: Monad m => RuntimeM m Outcome
latestHistory = do
  history <- gets stHistory
  if null history
    then throwError $ RuntimeError "no history to retrieve from"
    else return (head history)

latestState :: Monad m => RuntimeM m St
latestState = do
  outcome <- latestHistory
  case outcome of
    (Success state _) -> return state
    _ -> throwError $ RuntimeError "cannot retrieve state"

--------------------------------------------------------------------------------
-- | Commands

load :: (MonadIO m, Monad m) => FilePath -> RuntimeM m ()
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
        Just defns -> checkAll defns
        Nothing -> return ()


      env <- getEnv
      -- populate future, the next possible outcomes (there should be only 1)
      updateFuture $ interpret env initialState $ do
        _ <- call (Caller (PID False "you" (-1)) "main")
        return EffNoop
      -- retrieve state from the recently populated outcome and store it
      outcome <- selectedFuture
      case outcome of
        Success state reaction -> do
          pushHistory (Success state reaction)
          updateFuture $ interpret env state step
        Failure _ -> throwError $ RuntimeError "cannot retrieve outcome"

test :: (MonadIO m, Monad m) => RuntimeM m ()
test = do
  filePath <- getFilePath
  rawFile <- liftIO $ BS.readFile filePath
  case Parser.parseByteString2 filePath rawFile of
    Left err   -> error $ show err
    Right _  -> do
      latestState >>= liftIO . print


-- read and parse and store program from the stored filepath
reload :: (MonadIO m, Monad m) => RuntimeM m ()
reload = do
  filePath <- getFilePath
  load filePath

-- run the appointed outcome
run :: Monad m
  => RuntimeM m Val   -- input handler
  -> (Val -> RuntimeM m ())   -- output handler
  -> RuntimeM m ()
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
getFilePath :: Monad m => RuntimeM m FilePath
getFilePath = do
  result <- gets stFilePath
  case result of
    Nothing       -> throwError $ RuntimeError "please load the program first"
    Just filePath -> return filePath

getEnv :: Monad m => RuntimeM m Env
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
-- | Checkings

data TypeError = MissingProcDefn (Map ProcName Type)
  deriving (Show)

checkAll :: Monad m => Env -> RuntimeM m ()
checkAll env = do

  undefined

  -- -- type check those typed definitions
  -- env' <- gets stEnv
  -- error $ show env'
  --
  -- case env' of
  --   Just defns -> do
  --     defns' <- Map.traverseMaybeWithKey (const (return . withType)) defns
  --     error $ show defns'
  --     undefined
  --
  --   Nothing -> return ()



--------------------------------------------------------------------------------
-- | Checkings

programToEnv :: Monad m => Program -> RuntimeM m Env
programToEnv (Program declarations) = do
  -- throw if there is any type signature without a corresponding process definition
  unless (Map.null onlyTypes) $
    throwError $ TypeError $ MissingProcDefn onlyTypes

  return (Map.union withTypes withoutTypes)
  where
    toTypeSignPair (TypeSign n t) = Just (n, t)
    toTypeSignPair _              = Nothing

    toProcDefnPair (ProcDefn n t) = Just (n, t)
    toProcDefnPair _              = Nothing

    typeSigns = Map.fromList $ mapMaybe toTypeSignPair declarations
    procDefns = Map.fromList $ mapMaybe toProcDefnPair declarations

    withTypes :: Map ProcName DefnPair
    withTypes = fmap (uncurry WithType) $ Map.intersectionWith (,) procDefns typeSigns

    withoutTypes :: Map ProcName DefnPair
    withoutTypes =  fmap WithoutType $ Map.difference procDefns typeSigns

    onlyTypes :: Map ProcName Type
    onlyTypes =  Map.difference typeSigns procDefns

withType :: DefnPair -> Maybe (Pi, Type)
withType (WithType p t) = Just (p, t)
withType (WithoutType p) = Nothing
