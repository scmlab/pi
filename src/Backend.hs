module Backend where

import Control.Arrow ((***))
import Data.Text.Prettyprint.Doc

import Syntax
import PiMonad
import Interpreter

type BState =
  (Env, [Either ErrMsg (Res, BkSt)]) -- backend state

{-
commands:
 load
 trace
 run n
-}

start :: Env -> Pi -> BState
start defs p =
  (defs, map (fmap (Silent *** id))
      (runPiM 0 (lineup defs [p] ([],[],[],[]))))

down :: Int -> BState -> BState
down i (defs, sts) = down' (sts !! i)
  where
    down' (Left err) = (defs, [Left err])
    down' (Right (Output v p st, i)) =
      (defs, runPiM i (lineup defs [p] st >>= step defs))
    down' (Right (Input pps st, i)) =
      (defs, [Right (Input pps st, i)])
    down' (Right (Silent st, i)) =
      (defs, runPiM i (step defs st))

readInp :: Int -> Val -> BState -> BState
readInp i v (defs, sts) =
  case sts !! i of
    Right (Input pps st, j) ->
      (defs, runPiM j (Silent <$> input defs v pps st))
    _ -> error "not expecting input."

trace :: [Int] -> BState -> BState
trace [] = id
trace (i:is) = trace is . down i

ppBState :: BState -> Doc a
ppBState (_, sts) =
  vsep (map ppSts (zip [0..] sts))
 where ppSts (i,st) =
        vsep [pretty ("= " ++ show i ++ " ===="),
              ppMsgRes st,
              line]

--  ppBState . trace [0,0,0,0] $ start defs startE
