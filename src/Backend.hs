module Backend where

import Control.Arrow ((***))
import Data.Text.Prettyprint.Doc

import Syntax
import PiMonad
import Interpreter

-- backend state
data BState = BState Env [Either ErrMsg (Res, BkSt)]

{-
commands:
 load
 trace
 run n
-}

start :: Env -> Pi -> BState
start defs p =
  BState defs $ map (fmap (Silent *** id))
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
readInp i v (BState defs sts) =
  case sts !! i of
    Right (Input pps st, j) ->
      BState defs (runPiM j (Silent <$> input defs v pps st))
    _ -> error "not expecting input."

trace :: [Int] -> BState -> BState
trace []     = id
trace (i:is) = trace is . down i

instance Pretty BState where
  pretty (BState _ sts) = vsep (map ppSts (zip [0..] sts))
    where ppSts (i,st) = vsep [ pretty ("= " ++ show i ++ " ====")
                              , ppMsgRes st
                              , line]
