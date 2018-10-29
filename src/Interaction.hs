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


type Choice = Either ErrMsg (Res, BkSt)
data State = State
  { env     :: Env      -- source code
  , choices :: [Choice] -- choices of the next steps
  }
type Error = String
type InteractionM m = ExceptT Error (StateT State m)

data Request
  = Test Prog
  | Load Prog       -- load the program into the env
  | Run Int         -- choose and run the nth choice
  | Feed Int Val    -- feed the nth process with some value
  | Err String      -- report error
  deriving (Show)

data Response
  = ResChoices [Choice]
  | ResError   String
  -- deriving (Show)

initialEnv :: Env
initialEnv = []

--------------------------------------------------------------------------------
-- | Interaction Monad

-- start
runInteraction :: Monad m => Env -> Pi -> InteractionM m a -> m (Either Error a, State)
runInteraction env p handler = runStateT (runExceptT handler) (State env initialChoices)
  where initialChoices = map (fmap (Silent *** id))
          (runPiM 0 (lineup env [p] (St [] [] [] [])))

-- pretty print Choices
ppChoices :: [Choice] -> Doc n
ppChoices states = vsep (map ppSts (zip [0..] states))
    where ppSts (i,st) = vsep [ pretty ("==== " ++ show i ++ " ====")
                              , ppMsgRes st
                              , line]

-- safe (!!)
choose :: Monad m => Int -> InteractionM m Choice
choose i = do
  len <- length <$> gets choices
  if (i >= len) then
    throwError "out of bound"
  else
    (!! i) <$> gets choices

updateChoices :: Monad m => [Choice] -> InteractionM m ()
updateChoices new = modify (\state -> state { choices = new })

updateEnv :: Monad m => Env -> InteractionM m ()
updateEnv new = modify (\state -> state { env = new })

-- load
load :: Monad m => Env -> InteractionM m ()
load env = put $ State
  { env = env
  , choices = map (fmap (Silent *** id))
          (runPiM 0 (lineup env [Call (NS "main")] (St [] [] [] [])))
  }

-- down
run ::  Monad m => Int -> InteractionM m ()
run i = do
  choice <- choose i
  case choice of
    Left err ->
      updateChoices [Left err]
    Right (Output v p st, i) -> do
      defs <- gets env
      updateChoices $ runPiM i (lineup defs [p] st >>= step defs)
    Right (Input pps st, i) -> do
      updateChoices [Right (Input pps st, i)]
    Right (Silent st, i) -> do
      defs <- gets env
      updateChoices $ runPiM i (step defs st)

-- feed
feed :: Monad m => Int -> Val -> InteractionM m ()
feed i val = do
  choice <- choose i
  case choice of
    Right (Input pps st, j) -> do
      defs <- gets env
      updateChoices $ runPiM j (Silent <$> input defs val pps st)
    _ ->
      throwError "not expecting input"

--------------------------------------------------------------------------------
-- | BStates (legacy)

-- data BState = BState Env [Choice]
--
-- instance Pretty BState where
--   pretty (BState _ sts) = vsep (map ppSts (zip [0..] sts))
--     where ppSts (i,st) = vsep [ pretty ("= " ++ show i ++ " ====")
--                               , ppMsgRes st
--                               , line]
--
-- start :: Env -> Pi -> BState
-- start defs p = BState defs $ map (fmap (Silent *** id))
--   (runPiM 0 (lineup defs [p] ([],[],[],[])))
--
-- down :: Int -> BState -> BState
-- down i (BState defs sts) = down' (sts !! i)
--   where
--     down' (Left err) =
--       BState defs [Left err]
--     down' (Right (Output v p st, i)) =
--       BState defs (runPiM i (lineup defs [p] st >>= step defs))
--     down' (Right (Input pps st, i)) =
--       BState defs [Right (Input pps st, i)]
--     down' (Right (Silent st, i)) =
--       BState defs (runPiM i (step defs st))
--
-- readInp :: Int -> Val -> BState -> BState
-- readInp i v  (BState defs sts) =
--   case sts !! i of
--     Right (Input pps st, j) ->
--       BState defs (runPiM j (Silent <$> input defs v pps st))
--     _ -> error "not expecting input."
--
-- trace :: [Int] -> BState -> BState
-- trace []     = id
-- trace (i:is) = trace is . down i
