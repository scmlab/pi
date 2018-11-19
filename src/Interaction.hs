{-# LANGUAGE OverloadedStrings #-}

module Interaction where

import Control.Monad.State hiding (State, state)
import Control.Monad.Except
import Data.Text.Prettyprint.Doc
import qualified Data.ByteString.Lazy as BS

import Syntax.Abstract
import Syntax.Parser (ParseError(..), parseByteString)
import Interpreter


data Outcome = Success St Reaction BkSt
             | Failure ErrMsg
             deriving (Show)

instance Pretty Outcome where
  pretty (Failure msg)          = pretty ("error:" :: String) <+> pretty msg
  pretty (Success _ reaction _) = pretty reaction

data InteractionState = State
  { stFilePath :: Maybe String  -- loaded filepath
  , stEnv      :: Env           -- source code
  , stState    :: St            -- current state
  , stOutcomes :: [Outcome]     -- next possible outcomes
  , stCursor   :: Maybe Int     -- pointing at which outcome
  } deriving (Show)

type Error = String
type InteractionM m = ExceptT Error (StateT InteractionState m)

data Request
  = CursorMoveTo Int
  | CursorUp | CursorDown
  | CursorNext | CursorPrev
  | Help
  | Load String
  | Reload
  deriving (Show)

data Response
  = ResOutcomes     [Outcome]
  | ResTest         String
  | ResParseError   ParseError
  | ResOtherError String
  | ResNoOp
  -- deriving (Show)

--------------------------------------------------------------------------------
-- | Interaction Monad

runInteraction :: Monad m => InteractionM m a -> m (Either Error a, InteractionState)
runInteraction handler =
  runStateT (runExceptT handler) (State Nothing [] (St [] [] [] []) initialOutcomes Nothing)
  where initialOutcomes = [Failure "please load first"]

interpret :: Env -> BkSt -> PiMonad (St, Reaction) -> [Outcome]
interpret env i program = map toOutcome (runPiMonad env i program)
  where
    toOutcome :: Either String ((St, Reaction), BkSt) -> Outcome
    toOutcome (Left err)                     = Failure err
    toOutcome (Right ((state, reaction), j)) = Success state reaction j

update :: Monad m => [Outcome] -> InteractionM m ()
update outcomes = modify $ \state -> state
  { stOutcomes = outcomes
  , stCursor = if null outcomes then Nothing else Just 0
  }

--------------------------------------------------------------------------------
-- | Cursor related operations

withCursor :: Monad m => (Int -> InteractionM m a) -> InteractionM m a
withCursor f = do
  cursor <- gets stCursor
  case cursor of
    Nothing -> throwError "no outcomes to choose from"
    Just n -> f n

choose :: Monad m => Int -> InteractionM m ()
choose n = do
  len <- length <$> gets stOutcomes
  if (n >= len || n < 0) then
    throwError "out of bound"
  else do
    modify (\state -> state { stCursor = Just n })

-- retrieve the current appointed outcome
currentOutcome :: Monad m => InteractionM m Outcome
currentOutcome = do
  withCursor $ \n -> (!! n) <$> gets stOutcomes

currentState :: Monad m => InteractionM m St
currentState = gets stState

--------------------------------------------------------------------------------
-- | helper functions
toState :: Monad m => Outcome -> InteractionM m St
toState (Success state _ _) = return state
toState (Failure err) = throwError err

--------------------------------------------------------------------------------
-- | Commands

load :: (MonadIO m, Monad m) => FilePath -> InteractionM m ()
load filePath = do
  rawFile <- liftIO $ BS.readFile filePath
  case parseByteString rawFile of
    Left err          -> (throwError . show) err
    Right (Prog prog) -> do
      let env = map (\(PiDecl name p) -> (name, p)) prog
      let results = runPiMonad env 0 $ lineup [Call (NS "main")] (St [] [] [] [])
      case length results of
        0 -> throwError "failed to load the program"
        _ -> case (head results) of
          Left err -> throwError err
          Right (state, bk) -> put $ State
            { stFilePath = Just filePath
            , stEnv      = env
            , stState    = state
            , stOutcomes = [Success state Silent bk]
            , stCursor   = Just 0
            }

-- read and parse and store program from the stored filepath
reload :: (MonadIO m, Monad m) => InteractionM m ()
reload = do
  result <- gets stFilePath
  case result of
    Nothing -> throwError "please load the program first"
    Just filePath -> load filePath

-- run the appointed outcome
run :: Monad m => InteractionM m ()
run = do
  outcome <- currentOutcome
  case outcome of
    Failure err -> do
      update $ [Failure err]
    Success state (Output (Sender _ p)) i -> do
      defs <- gets stEnv
      update $ interpret defs i $ lineup [p] state >>= step
    Success state (React _ _ _ _) i -> do
      defs <- gets stEnv
      update $ interpret defs i (step state)
    Success state (Input pps) i -> do
      update $ [Success state (Input pps) i]
    Success state Silent i -> do
      defs <- gets stEnv
      -- error . show . pretty $ interpret defs i (step state)
      update $ interpret defs i (step state)

-- feed the appointed outcome with something
feed :: Monad m => Val -> InteractionM m ()
feed val = do
  outcome <- currentOutcome
  case outcome of
    Success state (Input pps) i -> do
      defs <- gets stEnv
      update $ interpret defs i $ do
        state' <- input val pps state
        return (state', Silent)
    _ ->
      throwError "not expecting input"
