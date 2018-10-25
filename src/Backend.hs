module Backend where

import Control.Arrow ((***))
import Control.Monad.State hiding (State)
import Control.Monad.Reader
import Control.Monad.Except
import Data.Text.Prettyprint.Doc

import Syntax
import PiMonad
import Interpreter

{-
commands:
 load
 trace
 run n
-}

type State = Either ErrMsg (Res, BkSt)
type Error = String
type InteractionM m = ExceptT Error (StateT [State] (ReaderT Env m))

data Request = Choose Int | Feed Int Val
  deriving (Show)

-- start
runInteraction :: Monad m => Env -> Pi -> InteractionM m a -> m (Either Error a, [State])
runInteraction env p handler = runReaderT (runStateT (runExceptT handler) initialState) env
  where initialState = map (fmap (Silent *** id))
          (runPiM 0 (lineup env [p] ([],[],[],[])))

ppStates :: [State] -> Doc n
ppStates states = vsep (map ppSts (zip [0..] states))
    where ppSts (i,st) = vsep [ pretty ("= " ++ show i ++ " ====")
                              , ppMsgRes st
                              , line]


-- safe (!!)
chooseState :: Monad m => Int -> InteractionM m State
chooseState i = do
  len <- gets length
  if (i >= len) then
    throwError "out of bound"
  else
    gets (!! i)

-- down
choose ::  Monad m => Int -> InteractionM m ()
choose i = do
  state <- chooseState i
  case state of
    Left err ->
      put [Left err]
    Right (Output v p st, i) -> do
      defs <- ask
      put $ runPiM i (lineup defs [p] st >>= step defs)
    Right (Input pps st, i) -> do
      put [Right (Input pps st, i)]
    Right (Silent st, i) -> do
      defs <- ask
      put $ runPiM i (step defs st)

-- feed
feed :: Monad m => Int -> Val -> InteractionM m ()
feed i val = do
  state <- chooseState i
  case state of
    Right (Input pps st, j) -> do
      defs <- ask
      put $ runPiM j (Silent <$> input defs val pps st)
    _ ->
      throwError "not expecting input"

--------------------------------------------------------------------------------

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
