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
  , stEnv      :: Env                 -- syntax tree
  , stState    :: St                  -- current state
  , stOutcomes :: [Outcome]           -- next possible outcomes
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

putEnv :: Monad m => Env -> InteractionM m ()
putEnv x = modify $ \ st -> st { stEnv = x }

putState :: Monad m => St -> InteractionM m ()
putState x = modify $ \ st -> st { stState = x }

putOutcome :: Monad m => [Outcome] -> InteractionM m ()
putOutcome outcomes = modify $ \ st -> st
  { stOutcomes = outcomes
  , stCursor = if null outcomes then Nothing else Just 0
  }

putCursor :: Monad m => Maybe Int -> InteractionM m ()
putCursor x = modify $ \ st -> st { stCursor = x }

--------------------------------------------------------------------------------

runInteraction :: Monad m => InteractionM m a -> m (Either Error a, InteractionState)
runInteraction handler =
  runStateT (runExceptT handler) (State Nothing Nothing [] (St [] [] [] []) initialOutcomes Nothing)
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
    Nothing -> throwError $ InteractionError "no outcomes to choose from"
    Just n -> f n

choose :: Monad m => Int -> InteractionM m ()
choose n = do
  len <- length <$> gets stOutcomes
  if (n >= len || n < 0) then
    throwError $ InteractionError "out of bound"
  else do
    putCursor (Just n)

-- retrieve the current appointed outcome
currentOutcome :: Monad m => InteractionM m Outcome
currentOutcome = do
  withCursor $ \n -> (!! n) <$> gets stOutcomes

currentState :: Monad m => InteractionM m St
currentState = gets stState

--------------------------------------------------------------------------------
-- | Commands

load :: (MonadIO m, Monad m) => FilePath -> InteractionM m ()
load filePath = do
  putFilePath (Just filePath)
  source <- liftIO $ BS.readFile filePath
  putSource (Just source)
  case Parser.parseByteString filePath source of
    Left err          -> throwError $ ParseError err
    Right (Prog prog) -> do
      let env = map (\(PiDecl name p) -> (ND (Pos name), p)) prog
      putEnv env
      let results = runPiMonad env 0 $ lineup [Call "main"] (St [] [] [] [])
      case length results of
        0 -> throwError $ InteractionError "failed to load the program"
        _ -> case (head results) of
          Left err -> throwError $ RuntimeError err
          Right (state, bk) -> do
            putState state
            putOutcome [Success state Silent bk]

test :: (MonadIO m, Monad m) => InteractionM m ()
test = do
  result <- gets stFilePath
  case result of
    Nothing -> throwError $ InteractionError "please load the program first"
    Just filePath -> do
      rawFile <- liftIO $ BS.readFile filePath
      case Parser.parseByteString2 filePath rawFile of
        Left err   -> error $ show err
        Right ast  -> liftIO $ print ast

-- read and parse and store program from the stored filepath
reload :: (MonadIO m, Monad m) => InteractionM m ()
reload = do
  result <- gets stFilePath
  case result of
    Nothing -> throwError $ InteractionError "please load the program first"
    Just filePath -> load filePath

-- run the appointed outcome
run :: Monad m => InteractionM m ()
run = do
  outcome <- currentOutcome
  case outcome of
    Failure err -> do
      putOutcome $ [Failure err]
    Success state (Output (Sender _ p)) i -> do
      defs <- gets stEnv
      putOutcome $ interpret defs i $ lineup [p] state >>= step
    Success state (React _ _ _ _) i -> do
      defs <- gets stEnv
      putOutcome $ interpret defs i (step state)
    Success state (Input pps) i -> do
      putOutcome $ [Success state (Input pps) i]
    Success state Silent i -> do
      defs <- gets stEnv
      -- error . show . pretty $ interpret defs i (step state)
      putOutcome $ interpret defs i (step state)

-- feed the appointed outcome with something
feed :: Monad m => Val -> InteractionM m ()
feed val = do
  outcome <- currentOutcome
  case outcome of
    Success state (Input pps) i -> do
      defs <- gets stEnv
      putOutcome $ interpret defs i $ do
        state' <- input val pps state
        return (state', Silent)
    _ ->
      throwError $ InteractionError "not expecting input"


--------------------------------------------------------------------------------
-- | Request

data Request
  = CursorMoveTo Int
  | CursorUp | CursorDown
  | CursorNext | CursorPrev
  | Help
  | Test
  | Load String
  | Reload
  deriving (Show)
