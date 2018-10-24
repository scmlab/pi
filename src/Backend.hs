module Backend where

import Control.Arrow ((***))
import Data.Text.Prettyprint.Doc

import Syntax
import PiMonad
import Interpreter

-- backend state
data BState = BState Env [Either ErrMsg (Res, BkSt)]

instance Pretty BState where
  pretty (BState _ sts) = vsep (map ppSts (zip [0..] sts))
    where ppSts (i,st) = vsep [ pretty ("= " ++ show i ++ " ====")
                              , ppMsgRes st
                              , line]

{-
commands:
 load
 trace
 run n
-}

data Request = Start Env Pi | Down Int | Read Int Val
  deriving (Show)

handleRequest :: Request -> BState -> BState
handleRequest (Start defs p) _ = BState defs $ map (fmap (Silent *** id))
  (runPiM 0 (lineup defs [p] ([],[],[],[])))
handleRequest (Down i) (BState defs sts) = down' (sts !! i)
  where
    down' (Left err) =
      BState defs [Left err]
    down' (Right (Output v p st, i)) =
      BState defs (runPiM i (lineup defs [p] st >>= step defs))
    down' (Right (Input pps st, i)) =
      BState defs [Right (Input pps st, i)]
    down' (Right (Silent st, i)) =
      BState defs (runPiM i (step defs st))
handleRequest (Read i v) (BState defs sts) =
  case sts !! i of
    Right (Input pps st, j) ->
      BState defs (runPiM j (Silent <$> input defs v pps st))
    _ -> error "not expecting input."

start :: Env -> Pi -> BState
start defs p = handleRequest (Start defs p) undefined

down :: Int -> BState -> BState
down i = handleRequest (Down i)

readInp :: Int -> Val -> BState -> BState
readInp i v = handleRequest (Read i v)

trace :: [Int] -> BState -> BState
trace []     = id
trace (i:is) = trace is . down i
