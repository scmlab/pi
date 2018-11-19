{-# LANGUAGE OverloadedStrings #-}

module Interaction where

import Control.Monad.State hiding (State, state)
import Control.Monad.Except
import Data.Text.Prettyprint.Doc

import Syntax.Abstract
import Syntax.Parser (ParseError(..))
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

-- appoint the nth possible outcome (unsafe)
appointUnsafe :: Monad m => Int -> InteractionM m ()
appointUnsafe n = modify (\state -> state { stCursor = Just n })

appoint :: Monad m => Int -> InteractionM m ()
appoint n = do
  len <- length <$> gets stOutcomes
  if (n >= len || n < 0) then
    throwError "out of bound"
  else do
    appointUnsafe n

-- retrieve the current appointed outcome
retrieveOutcome :: Monad m => InteractionM m Outcome
retrieveOutcome = do
  withCursor $ \n -> (!! n) <$> gets stOutcomes

currentState :: Monad m => InteractionM m St
currentState = gets stState

-- retrieve the nth outcome (and updates the cursor)
retrieveNthOutcome :: Monad m => Int -> InteractionM m Outcome
retrieveNthOutcome n = do
  appoint n
  retrieveOutcome

--------------------------------------------------------------------------------
-- | helper functions
toState :: Monad m => Outcome -> InteractionM m St
toState (Success state _ _) = return state
toState (Failure err) = throwError err

--------------------------------------------------------------------------------
-- | Commands

-- ReqLoad
load :: Monad m => FilePath -> Env -> InteractionM m ()
load filePath env = do
  let results = runPiMonad env 0 $ lineup [Call (NS "main")] (St [] [] [] [])
  case length results of
    0 -> throwError "failed to load the program"
    n -> case (head results) of
      Left err -> throwError err
      Right (state, bk) -> put $ State
        { stFilePath = Just filePath
        , stEnv      = env
        , stState    = state
        , stOutcomes = [Success state Silent bk]
        , stCursor   = Just 0
        }

-- ReqChoose: choose the nth outcome
choose :: Monad m => Int -> InteractionM m ()
choose = appoint

-- ReqRun: run the appointed outcome
run :: Monad m => InteractionM m ()
run = do
  outcome <- retrieveOutcome
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

-- ReqFeed: feed the appointed outcome with something
feed :: Monad m => Val -> InteractionM m ()
feed val = do
  outcome <- retrieveOutcome
  case outcome of
    Success state (Input pps) i -> do
      defs <- gets stEnv
      update $ interpret defs i $ do
        state' <- input val pps state
        return (state', Silent)
    _ ->
      throwError "not expecting input"
