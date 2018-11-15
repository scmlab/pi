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
  , stateOutcomes :: [Outcome]  -- outcomes
  } deriving (Show)
type Error = String
type InteractionM m = ExceptT Error (StateT State m)

data Request
  = Test
  | Load Prog       -- load the program into the env
  | Run Int         -- choose and run the nth choice
  | Feed Int Val    -- feed the nth process with some value
  | Err ParseError  -- error raised when parsing this request
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
runInteraction handler = runStateT (runExceptT handler) (State [] initialOutcomes)
  where initialOutcomes = interpret [] 0 $ hush $ lineup [Call (NS "main")] (St [] [] [] [])

interpret :: Env -> BkSt -> PiMonad (St, Reaction) -> [Outcome]
interpret env i program = map toOutcome (runPiMonad env i program)
  where
    toOutcome :: Either String ((St, Reaction), BkSt) -> Outcome
    toOutcome (Left err)                     = Failure err
    toOutcome (Right ((state, reaction), j)) = Success state reaction j

update :: Monad m => [Outcome] -> InteractionM m ()
update outcomes = modify (\state -> state { stateOutcomes = outcomes })

hush :: PiMonad St -> PiMonad (St, Reaction)
hush program = do
  state <- program
  return (state, Silent)

-- safe (!!)
decide :: Monad m => Int -> InteractionM m Outcome
decide i = do
  len <- length <$> gets stateOutcomes
  if (i >= len) then
    throwError "out of bound"
  else
    (!! i) <$> gets stateOutcomes

--------------------------------------------------------------------------------
-- | Commands

-- load
load :: Monad m => Env -> InteractionM m ()
load env = put $ State
  { stateEnv = env
  , stateOutcomes = interpret env 0 $ hush $ lineup [Call (NS "main")] (St [] [] [] [])
  }

-- down
run :: Monad m => Int -> InteractionM m ()
run n = do
  outcome <- decide n
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

-- feed
feed :: Monad m => Int -> Val -> InteractionM m ()
feed n val = do
  outcome <- decide n
  case outcome of
    Success state (Input pps) i -> do
      defs <- gets stateEnv
      update $ interpret defs i $ do
        state' <- input val pps state
        return (state', Silent)
    _ ->
      throwError "not expecting input"
