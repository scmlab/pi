module Interaction where
  -- ( Request(..)
  -- , Response(..)
  -- , InteractionM(..)
  -- , runInteraction
  -- , run
  -- , feed
  -- , ppStates
  -- ) where

import Control.Arrow ((***))
import Control.Monad.State hiding (State)
import Control.Monad.Except
import Data.Text.Prettyprint.Doc

import Syntax.Abstract
import PiMonad
import Interpreter


data State = State
  { env :: Env
  , states :: [Either ErrMsg (Res, BkSt)]
  }
 -- Either ErrMsg (Res, BkSt)
type Error = String
type InteractionM m = ExceptT Error (StateT State m)

data Request = Test Prog | Load Prog | Run Int | Feed Int Val | Err String
  deriving (Show)

data Response = ResError String | ResSuccess String
  deriving (Show)

initialEnv :: Env
initialEnv = []

initialStates :: Pi
initialStates = Call (NS "p0") `Par` Call (NS "p1")

-- liftIO :: IO a -> InteractionM IO a
-- liftIO = lift . lift
{-
commands:
 load
 trace
 run n
-}

--------------------------------------------------------------------------------
-- | Interaction Monad


-- start
runInteraction :: Monad m => Env -> Pi -> InteractionM m a -> m (Either Error a, State)
runInteraction env p handler = runStateT (runExceptT handler) (State env initialState)
  where initialState = map (fmap (Silent *** id))
          (runPiM 0 (lineup env [p] ([],[],[],[])))

ppStates :: [Either ErrMsg (Res, BkSt)] -> Doc n
ppStates states = vsep (map ppSts (zip [0..] states))
    where ppSts (i,st) = vsep [ pretty ("= " ++ show i ++ " ====")
                              , ppMsgRes st
                              , line]


-- safe (!!)
chooseState :: Monad m => Int -> InteractionM m (Either ErrMsg (Res, BkSt))
chooseState i = do
  len <- length <$> gets states
  if (i >= len) then
    throwError "out of bound"
  else
    (!! i) <$> gets states


updateStates :: Monad m => [Either ErrMsg (Res, BkSt)] -> InteractionM m ()
updateStates new = modify (\state -> state { states = new })

updateEnv :: Monad m => Env -> InteractionM m ()
updateEnv new = modify (\state -> state { env = new })

-- load
load :: Monad m => Env -> InteractionM m ()
load = updateEnv

-- down
run ::  Monad m => Int -> InteractionM m ()
run i = do
  state <- chooseState i
  case state of
    Left err ->
      updateStates [Left err]
    Right (Output v p st, i) -> do
      defs <- gets env
      updateStates $ runPiM i (lineup defs [p] st >>= step defs)
    Right (Input pps st, i) -> do
      updateStates [Right (Input pps st, i)]
    Right (Silent st, i) -> do
      defs <- gets env
      updateStates $ runPiM i (step defs st)

-- feed
feed :: Monad m => Int -> Val -> InteractionM m ()
feed i val = do
  state <- chooseState i
  case state of
    Right (Input pps st, j) -> do
      defs <- gets env
      updateStates $ runPiM j (Silent <$> input defs val pps st)
    _ ->
      throwError "not expecting input"


--------------------------------------------------------------------------------
-- | BStates

data BState = BState Env [Either ErrMsg (Res, BkSt)]

instance Pretty BState where
  pretty (BState _ sts) = vsep (map ppSts (zip [0..] sts))
    where ppSts (i,st) = vsep [ pretty ("= " ++ show i ++ " ====")
                              , ppMsgRes st
                              , line]

start :: Env -> Pi -> BState
start defs p = BState defs $ map (fmap (Silent *** id))
  (runPiM 0 (lineup defs [p] ([],[],[],[])))

down :: Int -> BState -> BState
down i (BState defs sts) = down' (sts !! i)
  where
    down' (Left err) =
      BState defs [Left err]
    down' (Right (Output v p st, i)) =
      BState defs (runPiM i (lineup defs [p] st >>= step defs))
    down' (Right (Input pps st, i)) =
      BState defs [Right (Input pps st, i)]
    down' (Right (Silent st, i)) =
      BState defs (runPiM i (step defs st))

readInp :: Int -> Val -> BState -> BState
readInp i v  (BState defs sts) =
  case sts !! i of
    Right (Input pps st, j) ->
      BState defs (runPiM j (Silent <$> input defs v pps st))
    _ -> error "not expecting input."

trace :: [Int] -> BState -> BState
trace []     = id
trace (i:is) = trace is . down i
