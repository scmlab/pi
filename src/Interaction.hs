{-# LANGUAGE OverloadedStrings #-}

module Interaction where

import Control.Monad.State hiding (State, state)
import Control.Monad.Except
import Data.Text.Prettyprint.Doc

import Syntax.Abstract
import Syntax.Parser (ParseError(..))
import Interpreter


type Outcome = Either ErrMsg ((St, Reaction), BkSt)
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

toOutcome :: Either String (St, BkSt) -> Outcome
toOutcome (Left err)       = Left err
toOutcome (Right (st, i))  = Right ((st, Silent), i)

-- start
runInteraction :: Monad m => Env -> Pi -> InteractionM m a -> m (Either Error a, State)
runInteraction env p handler = runStateT (runExceptT handler) (State env initialOutcomes)
  where initialOutcomes = map toOutcome $ runPiMonad env 0 (lineup [p] (St [] [] [] []))

-- pretty print Outcomes
ppOutcome :: Either ErrMsg ((St, Reaction), b) -> Doc a
ppOutcome (Left msg)       = pretty ("error:" :: String) <+> pretty msg
ppOutcome (Right (res, _)) = pretty res

ppOutcomes :: [Outcome] -> Doc n
ppOutcomes outcomes = vsep $
  [ pretty $ show (length outcomes) ++ " possible outcomes"
  ] ++ map ppSts (zip [0..] outcomes)
  where   ppSts :: (Int, Either ErrMsg ((St, Reaction), b)) -> Doc n
          ppSts (i, outcome) = vsep
            [ pretty ("==== " ++ show i ++ " ====")
            , ppOutcome outcome
            , line]

-- safe (!!)
decideOutcome :: Monad m => Int -> InteractionM m Outcome
decideOutcome i = do
  len <- length <$> gets stateOutcomes
  if (i >= len) then
    throwError "out of bound"
  else
    (!! i) <$> gets stateOutcomes

updateOutcomes :: Monad m => [Outcome] -> InteractionM m ()
updateOutcomes new = modify (\state -> state { stateOutcomes = new })

updateEnv :: Monad m => Env -> InteractionM m ()
updateEnv new = modify (\state -> state { stateEnv = new })

-- load
load :: Monad m => Env -> InteractionM m ()
load env = put $ State
  { stateEnv = env
  , stateOutcomes = map toOutcome (runPiMonad env 0 (lineup [Call (NS "main")] (St [] [] [] [])))
  }

-- down
run :: Monad m => Int -> InteractionM m ()
run n = do
  outcome <- decideOutcome n
  case outcome of
    Left err -> do
      updateOutcomes [Left err]
    Right ((state, Output (Sender _ p)), i) -> do
      defs <- gets stateEnv
      updateOutcomes $ runPiMonad defs i $ lineup [p] state >>= step
    Right ((state, React _ _ _ _), i) -> do
      defs <- gets stateEnv
      updateOutcomes $ runPiMonad defs i (step state)
    Right ((state, Input pps), i) -> do
      updateOutcomes [Right ((state, Input pps), i)]
    Right ((state, Silent), i) -> do
      defs <- gets stateEnv
      updateOutcomes $ runPiMonad defs i (step state)

-- feed
feed :: Monad m => Int -> Val -> InteractionM m ()
feed i val = do
  outcome <- decideOutcome i
  case outcome of
    Right ((state, Input pps), j) -> do
      defs <- gets stateEnv
      updateOutcomes $ runPiMonad defs j $ do
        state' <- input val pps state
        return (state', Silent)
    _ ->
      throwError "not expecting input"
