-- Shin-Cheng Mu, August 2018.
-- adapted from https://github.com/alexj136/pi/

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Interpreter where

import Control.Monad.State
import Control.Monad.Except
import Syntax
import PiMonad
import Utilities

type Env = FMap Name Pi

type St = ( [Pi]               -- processes running
          , FMap Name Waiting  -- processes waiting at each channels
          , [Name]             -- generated names
          )

stToPi :: St -> Pi
stToPi (ps, waits, news) =
  foldr Nu
    ((foldr Par End ps) `Par`
     (foldr Par End (map letWait waits))) news
  where letWait (c, Senders ps) =
          foldr1 Par [ Send (eN c) (EV v) p | (v,p) <- ps ]
        letWait (c, Receivers ps) =
          foldr1 Par [ Recv (eN c) pps | pps <- ps ]

data Waiting = Senders [(Val, Pi)]
             | Receivers [[(Ptrn, Pi)]]
   deriving Show
       -- non-empty

step :: (MonadFresh m, MonadError ErrMsg m) =>
        Env -> St -> m St
step defs ([], waits, news) = return ([], waits, news)  -- is this right?
step defs (End : ps, waits, news) = step defs (ps, waits, news)
step defs (Par p1 p2 : ps, waits, news) = step defs (p1:p2:ps, waits, news)
step defs (Send c v p : ps, waits, news) =
  evalExpr c >>= \c' -> evalExpr v >>= \v' ->
  doSend c' v' p (ps, waits, news)
step defs (Recv c pps : ps, waits, news) =
  evalExpr c >>= \c' ->
  doRecv c' pps (ps, waits, news)
step defs (Nu x p : ps, waits, news) = doNu x p (ps, waits, news)
step defs (Call x : ps, waits, news)
  | Just p <- lookup x defs = step defs (p:ps, waits, news)
  | otherwise = throwError "definition not found"

doSend :: MonadError ErrMsg m =>
          Val -> Val -> Pi -> St -> m St
doSend (N c) v p (ps, waits, news) =
  case lookup c waits of
    Nothing -> return (ps, (c, Senders [(v,p)]):waits, news)
    Just (Senders _) ->
      return (ps, fMapUpdate c (addSender v p) waits, news)
    Just (Receivers (pqs:qs)) ->
       case matchPPs pqs v of
         Just (th, q) ->
             return ([substPi th q] ++ ps ++ [p]
                    , popReceiver c qs waits, news)
         Nothing -> throwError "pattern matching fails"
  where addSender v p (Senders qs) = Senders ((v,p):qs)
        addSender v p (Receivers _) = error "shouldn't happen"
        popReceiver c [] = rmEntry c
        popReceiver c qs = fMapUpdate c (const (Receivers qs))
doSend _ _ _ _  = throwError "type error in send"

doRecv :: MonadError ErrMsg m =>
          Val -> [(Ptrn, Pi)] -> St -> m St
doRecv (N c) pps (ps, waits, news) =
  case lookup c waits of
    Nothing -> return (ps, (c, Receivers [pps]):waits, news)
    Just (Receivers _) ->
       return (ps, fMapUpdate c (addReceiver pps) waits, news)
    Just (Senders ((v,q):qs)) ->
       case matchPPs pps v of
        Just (th, p) ->
          return ( [q] ++ ps ++ [substPi th p]
                 , popSender c qs waits, news)
        Nothing -> throwError "pattern matching fails"
  where addReceiver pps (Receivers qs) = Receivers (pps:qs)
        addReceiver pps (Senders _) = error "shouldn't happen"
        popSender c [] = rmEntry c
        popSender c qs = fMapUpdate c (const (Senders qs))
doRecv _ _ _  = throwError "type error in recv"

doNu :: MonadFresh m => Name -> Pi -> St -> m St
doNu x p (ps, waits, news) =
  fresh >>= \i ->
  return (substPi [(x, N i)] p : ps, waits, i : news)
