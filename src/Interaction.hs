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

data State = State
  { stateEnv      :: Env        -- source code
  , stateState    :: St         -- current state
  , stateOutcomes :: [Outcome]  -- next possible outcomes
  , stateCursor   :: Maybe Int        -- pointing at which outcome
  } deriving (Show)
type Error = String
type InteractionM m = ExceptT Error (StateT State m)

data Request
  = ReqTest
  | ReqLoad Prog          -- load the program into the env
  | ReqChoose Int         -- choose the nth outcome
  | ReqRun                -- run the appointed outcome
  | ReqFeed Val           -- feed the appointed outcome with something
  | ReqErr ParseError     -- error raised when parsing this request
  deriving (Show)

data Response
  = ResOutcomes     [Outcome]
  | ResTest         String
  | ResParseError   ParseError
  | ResGenericError String
  -- deriving (Show)

--------------------------------------------------------------------------------
-- | Interaction Monad

runInteraction :: Monad m => InteractionM m a -> m (Either Error a, State)
runInteraction handler =
  runStateT (runExceptT handler) (State [] (St [] [] [] []) initialOutcomes Nothing)
  where initialOutcomes = [Failure "please load first"]

interpret :: Env -> BkSt -> PiMonad (St, Reaction) -> [Outcome]
interpret env i program = map toOutcome (runPiMonad env i program)
  where
    toOutcome :: Either String ((St, Reaction), BkSt) -> Outcome
    toOutcome (Left err)                     = Failure err
    toOutcome (Right ((state, reaction), j)) = Success state reaction j

update :: Monad m => [Outcome] -> InteractionM m ()
update outcomes = modify $ \state -> state
  { stateOutcomes = outcomes
  , stateCursor = if null outcomes then Nothing else Just 0
  }

--------------------------------------------------------------------------------
-- | Cursor related operations

withCursor :: Monad m => (Int -> InteractionM m a) -> InteractionM m a
withCursor f = do
  cursor <- gets stateCursor
  case cursor of
    Nothing -> throwError "no outcomes to choose from"
    Just n -> f n

-- appoint the nth possible outcome (unsafe)
appointUnsafe :: Monad m => Int -> InteractionM m ()
appointUnsafe n = modify (\state -> state { stateCursor = Just n })

appoint :: Monad m => Int -> InteractionM m ()
appoint n = do
  len <- length <$> gets stateOutcomes
  if (n >= len || n < 0) then
    throwError "out of bound"
  else do
    appointUnsafe n

-- move the cursor up and down (safe)
next :: Monad m => InteractionM m Int
next = do
  len <- length <$> gets stateOutcomes
  withCursor $ \n ->
    if (n + 1 < len)
      then appointUnsafe (n + 1) >> return (n + 1)
      else                          return n

prev :: Monad m => InteractionM m Int
prev = do
  withCursor $ \n ->
    if (n > 0)
      then appointUnsafe (n - 1) >> return (n - 1)
      else                          return n

-- retrieve the current appointed outcome
retrieve :: Monad m => InteractionM m Outcome
retrieve = do
  withCursor $ \n -> (!! n) <$> gets stateOutcomes

-- retrieve the nth outcome (and updates the cursor)
retrieveNth :: Monad m => Int -> InteractionM m Outcome
retrieveNth n = do
  appoint n
  retrieve

--------------------------------------------------------------------------------
-- | helper functions
toState :: Monad m => Outcome -> InteractionM m St
toState (Success state _ _) = return state
toState (Failure err) = throwError err

hush :: PiMonad St -> PiMonad (St, Reaction)
hush program = do
  state <- program
  return (state, Silent)


--------------------------------------------------------------------------------
-- | Commands

-- ReqLoad
load :: Monad m => Env -> InteractionM m ()
load env = do
  let outcomes = interpret env 0 $ hush $ lineup [Call (NS "main")] (St [] [] [] [])
  put $ State
    { stateEnv      = env
    , stateState    = St [] [] [] []
    , stateOutcomes = outcomes
    , stateCursor   = if null outcomes then Nothing else Just 0
    }

-- ReqChoose: choose the nth outcome
choose :: Monad m => Int -> InteractionM m ()
choose = appoint

-- ReqRun: run the appointed outcome
run :: Monad m => InteractionM m ()
run = do
  outcome <- retrieve
  case outcome of
    Failure err -> do
      update $ [Failure err]
    Success state (Output (Sender _ p)) i -> do
      defs <- gets stateEnv
      update $ interpret defs i $ lineup [p] state >>= step
    Success state (React _ _ _ _) i -> do
      defs <- gets stateEnv
      update $ interpret defs i (step state)
    Success state (Input pps) i -> do
      update $ [Success state (Input pps) i]
    Success state Silent i -> do
      defs <- gets stateEnv
      update $ interpret defs i (step state)

-- ReqFeed: feed the appointed outcome with something
feed :: Monad m => Val -> InteractionM m ()
feed val = do
  outcome <- retrieve
  case outcome of
    Success state (Input pps) i -> do
      defs <- gets stateEnv
      update $ interpret defs i $ do
        state' <- input val pps state
        return (state', Silent)
    _ ->
      throwError "not expecting input"
