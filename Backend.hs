module Backend where

import Syntax
import PiMonad
import Interpreter

type BState = (Env, St, BkSt) -- backend state

{-
commands:
 load
 trace
 run n
-}

data Trace = Stop
           | Deadlock St
           | Error ErrMsg
           | TOut Val BState
           | TIn BState
           | Next St Trace

eqSt :: St -> St -> Bool
eqSt = (==)

trace1 :: BState -> Trace
trace1 (defs, st, bk) = Next st (trace (defs, st, bk))

trace :: BState -> Trace
trace (defs, st, bk) | stopped st = Stop
trace (defs, st, bk) =
  case runPiM bk (step defs st) of
    Left errMsg -> Error errMsg
    Right (Silent st', bk') ->
      if eqSt st st' then Deadlock st'
         else Next st' (trace (defs, st', bk'))
    Right (Output st' v, bk') ->
      TOut v (defs, st', bk')
    Right (Input st', bk') -> TIn (defs, st', bk')
