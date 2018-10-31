{-# LANGUAGE OverloadedStrings #-}

module Interaction where

import Control.Arrow ((***))
import Control.Monad.State hiding (State, state)
import Control.Monad.Except
import Data.Text.Prettyprint.Doc

import Syntax.Abstract
import Syntax.Concrete (ParseError)
import Interpreter


type Outcome = Either ErrMsg (Reaction, BkSt)
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
toOutcome (Right (st, i))  = Right (Silent st, i)

-- start
runInteraction :: Monad m => Env -> Pi -> InteractionM m a -> m (Either Error a, State)
runInteraction env p handler = runStateT (runExceptT handler) (State env initialOutcomes)
  where initialOutcomes = map toOutcome $ runPiMonad env 0 (lineup [p] (St [] [] [] []))

-- pretty print Outcomes
ppOutcome :: Either ErrMsg (Reaction, b) -> Doc a
ppOutcome (Left msg)       = pretty ("error:" :: String) <+> pretty msg
ppOutcome (Right (res, _)) = pretty res

ppOutcomes :: [Outcome] -> Doc n
ppOutcomes states = vsep (map ppSts (zip [0..] states))
    where   ppSts :: (Int, Either ErrMsg (Reaction, b)) -> Doc n
            ppSts (i,st) = vsep [ pretty ("==== " ++ show i ++ " ====")
                              , ppOutcome st
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
  , stateOutcomes = map (fmap (Silent *** id))
          (runPiMonad env 0 (lineup [Call (NS "main")] (St [] [] [] [])))
  }

-- down
run :: Monad m => Int -> InteractionM m ()
run n = do
  outcome <- decideOutcome n
  case outcome of
    Left err ->
      updateOutcomes [Left err]
    Right (Output state (Sender _ p), i) -> do
      defs <- gets stateEnv
      updateOutcomes $ runPiMonad defs i $ lineup [p] state >>= step
    Right (React state _ _ _ _, i) -> do
      defs <- gets stateEnv
      updateOutcomes $ runPiMonad defs i (step state)
    Right (Input state pps, i) -> do
      updateOutcomes [Right (Input state pps, i)]
    Right (Silent state, i) -> do
      defs <- gets stateEnv
      updateOutcomes $ runPiMonad defs i (step state)

-- feed
feed :: Monad m => Int -> Val -> InteractionM m ()
feed i val = do
  outcome <- decideOutcome i
  case outcome of
    Right (Input st pps, j) -> do
      defs <- gets stateEnv
      updateOutcomes $ runPiMonad defs j (Silent <$> input val pps st)
    _ ->
      throwError "not expecting input"

--------------------------------------------------------------------------------
-- | BStates (legacy)

-- data BState = BState Env [Choice]
--
-- instance Pretty BState where
--   pretty (BState _ sts) = vsep (map ppSts (zip [0..] sts))
--     where ppSts (i,st) = vsep [ pretty ("= " ++ show i ++ " ====")
--                               , ppOutcome st
--                               , line]
--
-- start :: Env -> Pi -> BState
-- start defs p = BState defs $ map (fmap (Silent *** id))
--   (runPiMonad 0 (lineup defs [p] ([],[],[],[])))
--
-- down :: Int -> BState -> BState
-- down i (BState defs sts) = down' (sts !! i)
--   where
--     down' (Left err) =
--       BState defs [Left err]
--     down' (Right (Output v p st, i)) =
--       BState defs (runPiMonad i (lineup defs [p] st >>= step defs))
--     down' (Right (Input pps st, i)) =
--       BState defs [Right (Input pps st, i)]
--     down' (Right (Silent st, i)) =
--       BState defs (runPiMonad i (step defs st))
--
-- readInp :: Int -> Val -> BState -> BState
-- readInp i v  (BState defs sts) =
--   case sts !! i of
--     Right (Input pps st, j) ->
--       BState defs (runPiMonad j (Silent <$> input defs v pps st))
--     _ -> error "not expecting input."
--
-- trace :: [Int] -> BState -> BState
-- trace []     = id
-- trace (i:is) = trace is . down i
