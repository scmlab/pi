-- Shin-Cheng Mu, August 2018.
-- adapted from https://github.com/alexj136/pi/

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Interpreter where

import Control.Monad.State
import Control.Monad.Except
import Control.Arrow ((***))
import Syntax
import PiMonad
import Utilities

type Env = FMap Name Pi

type St = ( [Pi]               -- processes running
          , FMap Name Waiting  -- processes waiting at each channels
          , [Name]             -- generated names
          , Maybe Val          -- stdout
          )

stToPi :: St -> Pi
stToPi (ps, waits, news, _) =
  foldr Nu
    ((foldr Par End ps) `Par`
     (foldr Par End (map letWait waits))) news
  where letWait (c, Senders ps) =
          foldr1 Par [ Send c (EV v) p | (v,p) <- ps ]
        letWait (c, Receivers ps) =
          foldr1 Par [ Recv c pps | pps <- ps ]

data Waiting = Senders [(Val, Pi)]
             | Receivers [[(Ptrn, Pi)]]
   deriving Show
       -- non-empty

step :: (MonadFresh m, MonadError ErrMsg m) =>
        Env -> St -> m St
step defs ([], waits, news, sout) =
  return ([], waits, news, sout)  -- is this right?
step defs (End : ps, waits, news, sout) =
  step defs (ps, waits, news, sout)
step defs (Par p1 p2 : ps, waits, news, sout) =
  step defs (p1:p2:ps, waits, news, sout)
step defs (Send c x p : ps, waits, news, sout) =
  evalExpr x >>= \v ->
  doSend c v p (ps, waits, news, sout)
step defs (Recv c pps : ps, waits, news, sout) =
  doRecv c pps (ps, waits, news, sout)
step defs (Nu x p : ps, waits, news, sout) =
  doNu x p (ps, waits, news, sout)
step defs (Call x : ps, waits, news, sout)
  | Just p <- lookup x defs =
    step defs (p:ps, waits, news, sout)
  | otherwise = throwError "definition not found"

doSend :: MonadError ErrMsg m =>
          Name -> Val -> Pi -> St -> m St
doSend (NR StdOut) v p (ps, waits, news, _) =
  return (p:ps, waits, news, Just v)
doSend c v p (ps, waits, news, sout) =
  case lookup c waits of
    Nothing ->
      return (ps, (c, Senders [(v,p)]):waits, news, sout)
    Just (Senders _) ->
      return (ps, fMapUpdate c (addSender v p) waits, news, sout)
    Just (Receivers (pqs:qs)) ->
       case matchPPs pqs v of
         Just (th, q) ->
             return ([substPi th q] ++ ps ++ [p]
                    , popReceiver c qs waits, news, sout)
         Nothing -> throwError "pattern matching fails"
  where addSender v p (Senders qs) = Senders ((v,p):qs)
        addSender v p (Receivers _) = error "shouldn't happen"
        popReceiver c [] = rmEntry c
        popReceiver c qs = fMapUpdate c (const (Receivers qs))

doRecv :: MonadError ErrMsg m =>
          Name -> [(Ptrn, Pi)] -> St -> m St
doRecv c pps (ps, waits, news, sout) =
  case lookup c waits of
    Nothing ->
      return (ps, (c, Receivers [pps]):waits, news, sout)
    Just (Receivers _) ->
       return (ps, fMapUpdate c (addReceiver pps) waits, news, sout)
    Just (Senders ((v,q):qs)) ->
       case matchPPs pps v of
        Just (th, p) ->
          return ( [q] ++ ps ++ [substPi th p]
                 , popSender c qs waits, news, sout)
        Nothing -> throwError "pattern matching fails"
  where addReceiver pps (Receivers qs) = Receivers (pps:qs)
        addReceiver pps (Senders _) = error "shouldn't happen"
        popSender c [] = rmEntry c
        popSender c qs = fMapUpdate c (const (Senders qs))

doNu :: MonadFresh m => Name -> Pi -> St -> m St
doNu x p (ps, waits, news, sout) =
  fresh >>= \i ->
  return (substPi [(x, N i)] p : ps, waits, i : news, sout)

iterateM :: Monad m => (a -> m a) -> a -> m [a]
iterateM f x = (f x >>= iterateM f) >>= (return . (x:))

trace :: Env -> Int -> St -> [Either String St]
trace defs i st =
  case runStateT (step defs st :: PiMonad St) i of
      Left err -> [Left err]
      Right (st', i') -> Right st' : trace defs i' st'

run :: Env -> Int -> Int -> St -> ([Val], Either ErrMsg St)
run _ 0 _ st = ([],Right st)
run defs n i st =
  case runStateT (step defs st :: PiMonad St) i of
    Left err -> ([], Left err)
    Right ((ps,waits,news,Just v), i') ->
      ((v:) *** id)
        (run defs (n-1) i' (ps,waits,news,Nothing))
    Right (st', i') -> run defs (n-1) i' st'
