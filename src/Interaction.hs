{-# LANGUAGE OverloadedStrings #-}

module Interaction where

import Control.Monad.State hiding (State, state)
import Control.Monad.Except
import Data.Text.Prettyprint.Doc
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import Syntax.Abstract
-- import Syntax.Concrete (restore)
import qualified Syntax.Parser as Parser
import Interpreter


 --------------------------------------------------------------------------------
 -- | Interaction Monad

data Outcome = Success St Reaction BkSt
             | Failure ErrMsg
             deriving (Show)

instance Pretty Outcome where
  pretty (Failure msg)          = pretty ("error:" :: String) <+> pretty msg
  pretty (Success _ reaction _) = pretty reaction

data InteractionState = State
  { stFilePath :: Maybe String        -- loaded filepath
  , stSource   :: Maybe ByteString    -- source code
  , stEnv      :: Maybe Env           -- syntax tree
  , stHistory  :: [Outcome]           -- history outcomes
  , stFuture   :: [Outcome]           -- next possible outcomes
  , stCursor   :: Maybe Int           -- pointing at which outcome
  } deriving (Show)

data Error = ParseError Parser.ParseError
           | TypeError String
           | RuntimeError String
           | InteractionError String
           deriving (Show)

type InteractionM m = ExceptT Error (StateT InteractionState m)

--------------------------------------------------------------------------------

putFilePath :: Monad m => Maybe FilePath -> InteractionM m ()
putFilePath x = modify $ \ st -> st { stFilePath = x }

putSource :: Monad m => Maybe ByteString -> InteractionM m ()
putSource x = modify $ \ st -> st { stSource = x }

putEnv :: Monad m => Maybe Env -> InteractionM m ()
putEnv x = modify $ \ st -> st { stEnv = x }

pushHistory :: Monad m => Outcome -> InteractionM m ()
pushHistory x = do
  xs <- gets stHistory
  modify $ \st -> st { stHistory = x:xs }

updateFuture :: Monad m => [Outcome] -> InteractionM m ()
updateFuture outcomes = modify $ \ st -> st
  { stFuture = outcomes
  , stCursor = if null outcomes then Nothing else Just 0
  }

putCursor :: Monad m => Maybe Int -> InteractionM m ()
putCursor x = modify $ \ st -> st { stCursor = x }

--------------------------------------------------------------------------------

runInteraction :: Monad m => InteractionM m a -> m (Either Error a, InteractionState)
runInteraction handler =
  runStateT (runExceptT handler) (State Nothing Nothing Nothing [] initialOutcomes Nothing)
  where initialOutcomes = [Failure "please load first"]

interpret :: Env -> BkSt -> PiMonad (St, Reaction) -> [Outcome]
interpret env i program = map toOutcome (runPiMonad env i program)
  where
    toOutcome :: Either String ((St, Reaction), BkSt) -> Outcome
    toOutcome (Left err)                     = Failure err
    toOutcome (Right ((state, reaction), j)) = Success state reaction j


--------------------------------------------------------------------------------
-- | Cursor related operations

withCursor :: Monad m => (Int -> InteractionM m a) -> InteractionM m a
withCursor f = do
  cursor <- gets stCursor
  case cursor of
    Nothing -> throwError $ InteractionError "cannot go any further"
    Just n -> f n

choose :: Monad m => Int -> InteractionM m ()
choose n = do
  len <- length <$> gets stFuture
  if (n >= len || n < 0) then
    throwError $ InteractionError "out of bound"
  else do
    putCursor (Just n)

-- retrieve the next appointed outcome
selectedFuture :: Monad m => InteractionM m Outcome
selectedFuture = do
  withCursor $ \n -> (!! n) <$> gets stFuture

latestHistory :: Monad m => InteractionM m Outcome
latestHistory = do
  history <- gets stHistory
  if null history
    then throwError $ InteractionError "no history to retrieve from"
    else return (head history)

latestState :: Monad m => InteractionM m St
latestState = do
  outcome <- latestHistory
  case outcome of
    (Success state _ _) -> return state
    _ -> throwError $ InteractionError "cannot retrieve state"

--------------------------------------------------------------------------------
-- | Commands

load :: (MonadIO m, Monad m) => FilePath -> InteractionM m ()
load filePath = do
  -- storing the filepath
  putFilePath (Just filePath)
  -- storing the source
  source <- liftIO $ BS.readFile filePath
  putSource (Just source)
  -- parse and store the AST
  case Parser.parseByteString filePath source of
    Left err  -> throwError $ ParseError err
    Right ast -> (putEnv . Just . programToEnv) ast

  env <- getEnv
  -- populate future, the next possible outcomes (there should be only 1)
  updateFuture $ interpret env 0 $ do
    state <- lineup [Call "main"] (St [] [] [] [] 0)
    return (state, Silent)
  -- retrieve state from the recently populated outcome and store it
  outcome <- selectedFuture
  case outcome of
    Success state reaction bk -> do
      pushHistory (Success state reaction bk)
      updateFuture $ interpret env bk (step state)
    Failure err -> throwError $ InteractionError "cannot retrieve outcome"

test :: (MonadIO m, Monad m) => InteractionM m ()
test = do
  filePath <- getFilePath
  rawFile <- liftIO $ BS.readFile filePath
  case Parser.parseByteString2 filePath rawFile of
    Left err   -> error $ show err
    Right ast  -> do
      latestState >>= liftIO . print


-- read and parse and store program from the stored filepath
reload :: (MonadIO m, Monad m) => InteractionM m ()
reload = do
  filePath <- getFilePath
  load filePath

-- run the appointed outcome
run :: (MonadIO m, Monad m)
  => InteractionM m Val   -- input handler
  -> (Val -> InteractionM m ())   -- output handler
  -> InteractionM m ()
run inputHandler outputHandler = do
  outcome <- selectedFuture
  pushHistory outcome
  case outcome of
    Failure err -> do
      updateFuture $ [Failure err]
    Success oldState (Output (Sender _ val p)) i -> do
      env <- getEnv
      updateFuture $ interpret env i $ do
        lineup [p] oldState >>= step
      -- handle output
      outputHandler val
    Success oldState (React _ _ _) i -> do
      env <- getEnv
      updateFuture $ interpret env i (step oldState)
    Success oldState (Input pps) i -> do
      -- handle input
      val <- inputHandler
      env <- getEnv
      updateFuture $ interpret env i $ do
        state' <- input val pps oldState
        return (state', Silent)
    Success oldState Silent i -> do
      env <- getEnv
      updateFuture $ interpret env i (step oldState)

--------------------------------------------------------------------------------
-- | Helpers

-- get existing filepath from the state
getFilePath :: Monad m => InteractionM m FilePath
getFilePath = do
  result <- gets stFilePath
  case result of
    Nothing       -> throwError $ InteractionError "please load the program first"
    Just filePath -> return filePath

getEnv :: Monad m => InteractionM m Env
getEnv = do
  result <- gets stEnv
  case result of
    Nothing   -> throwError $ InteractionError "panic: the AST has not been parsed and stored"
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
  deriving (Show)
