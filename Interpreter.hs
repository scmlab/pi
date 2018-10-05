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
          )

data Waiting = Senders [(Val, Pi)]
             | Receivers [[(Ptrn, Pi)]]
   deriving (Eq, Show)
       -- non-empty

stopped :: St -> Bool
stopped (ps, waits, news) =
    null ps && and (map (nullW . snd) waits)
  where nullW (Senders ps) = null ps
        nullW (Receivers pps) = null pps

stToPi :: St -> Pi
stToPi (ps, waits, news) =
  foldr Nu
    ((foldr par End ps) `par`
     (foldr par End (map letWait waits))) news
  where letWait (c, Senders ps) =
          foldr1 par [ Send c (EV v) p | (v,p) <- ps ]
        letWait (c, Receivers ps) =
          foldr1 par [ Recv c pps | pps <- ps ]

data Res a = Silent a
           | Output a Val
           | Input a

step :: (MonadFresh m, MonadError ErrMsg m) =>
        Env -> St -> m (Res St)
step defs ([], waits, news) =
  return . Silent $ ([], waits, news)  -- is this right?
step defs (End : ps, waits, news) =
  step defs (ps, waits, news)
step defs (Par p1 p2 : ps, waits, news) =
  step defs (p1:p2:ps, waits, news)
step defs (Send c x p : ps, waits, news) =
  evalExpr x >>= \v ->
  doSend c v p (ps, waits, news)
step defs (Recv c pps : ps, waits, news) =
  doRecv c pps (ps, waits, news)
step defs (Nu x p : ps, waits, news) =
  doNu x p (ps, waits, news)
step defs (Call x : ps, waits, news)
  | Just p <- lookup x defs =
    step defs (p:ps, waits, news)
  | otherwise = throwError "definition not found"

doSend :: MonadError ErrMsg m =>
          Name -> Val -> Pi -> St -> m (Res St)
doSend (NR StdOut) v p (ps, waits, news) =
  return (Output (p:ps, waits, news) v)
doSend c v p (ps, waits, news) =
  case lookup c waits of
    Nothing ->
      return . Silent $ (ps, (c, Senders [(v,p)]):waits, news)
    Just (Senders _) ->
      return . Silent $ (ps, fMapUpdate c (addSender v p) waits, news)
    Just (Receivers (pqs:qs)) ->
       case matchPPs pqs v of
         Just (th, q) ->
             return . Silent $ ([substPi th q] ++ ps ++ [p]
                       , popReceiver c qs waits, news)
         Nothing -> throwError "pattern matching fails"
  where addSender v p (Senders qs) = Senders ((v,p):qs)
        addSender v p (Receivers _) = error "shouldn't happen"
        popReceiver c [] = rmEntry c
        popReceiver c qs = fMapUpdate c (const (Receivers qs))

doRecv :: MonadError ErrMsg m =>
          Name -> [(Ptrn, Pi)] -> St -> m (Res St)
doRecv c pps (ps, waits, news) =
  case lookup c waits of
    Nothing ->
      return . Silent $ (ps, (c, Receivers [pps]):waits, news)
    Just (Receivers _) ->
      return . Silent $ (ps, fMapUpdate c (addReceiver pps) waits, news)
    Just (Senders ((v,q):qs)) ->
       case matchPPs pps v of
        Just (th, p) ->
          return . Silent$ ( [q] ++ ps ++ [substPi th p]
                 , popSender c qs waits, news)
        Nothing -> throwError "pattern matching fails"
  where addReceiver pps (Receivers qs) = Receivers (pps:qs)
        addReceiver pps (Senders _) = error "shouldn't happen"
        popSender c [] = rmEntry c
        popSender c qs = fMapUpdate c (const (Senders qs))

doNu :: MonadFresh m => Name -> Pi -> St -> m (Res St)
doNu x p (ps, waits, news) =
  fresh >>= \i ->
  return . Silent $ (substPi [(x, N i)] p : ps, waits, i : news)
{-
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
-}
