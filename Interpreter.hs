-- Shin-Cheng Mu, August 2018.
-- adapted from https://github.com/alexj136/pi/

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Interpreter where

import Control.Monad.State
import Syntax
import PiMonad
import Utilities

type St = ( [Pi]               -- processes running
          , FMap Name Waiting    -- processes waiting at each channels
          , [Name]               -- generated names
          )

stToPi :: St -> Pi
stToPi (ps, waits, news) =
  foldr Nu
    ((foldr Par End ps) `Par`
     (foldr Par End (map letWait waits))) news
  where letWait (c, Senders ps) =
          foldr1 Par [ Send (eN c) (EV v) p | (v,p) <- ps ]
        letWait (c, Receivers ps) =
          foldr1 Par [ Recv (eN c) x p | (x,p) <- ps ]

data Waiting = Senders [(Val, Pi)] | Receivers [(Ptrn, Pi)]
   deriving Show
       -- non-empty

step :: MonadFresh m => St -> m St
step ([], waits, news) = return ([], waits, news)  -- is this right?
step (End : ps, waits, news) = step (ps, waits, news)
step (Par p1 p2 : ps, waits, news) = step (p1:p2:ps, waits, news)
step (Send c v p : ps, waits, news) =
  return $ doSend (evalExpr c) (evalExpr v) p (ps, waits, news)
step (Recv c xs p : ps, waits, news) =
  return $ doRecv (evalExpr c) xs p (ps, waits, news)
step (Nu x p : ps, waits, news) = doNu x p (ps, waits, news)

doSend :: Val -> Val -> Pi -> St -> St
doSend (N c) v p (ps, waits, news) =
  case lookup c waits of
    Nothing -> (ps, (c, Senders [(v,p)]):waits, news)
    Just (Senders _) -> (ps, fMapUpdate c (addSender v p) waits, news)
    Just (Receivers ((xs,q):qs)) ->
            ( [substPi (match xs v) q] ++ ps ++ [p]
            , popReceiver c qs waits, news)
  where addSender v p (Senders qs) = Senders ((v,p):qs)
        addSender v p (Receivers _) = error "shouldn't happen"
        popReceiver c [] = rmEntry c
        popReceiver c qs = fMapUpdate c (const (Receivers qs))
doSend _ _ _ _  = error "type error in send"

doRecv :: Val -> Ptrn -> Pi -> St -> St
doRecv (N c) xs p (ps, waits, news) =
  case lookup c waits of
    Nothing -> (ps, (c, Receivers [(xs,p)]):waits, news)
    Just (Receivers _) -> (ps, fMapUpdate c (addReceiver xs p) waits, news)
    Just (Senders ((v,q):qs)) ->
       ( [q] ++ ps ++ [substPi (match xs v) p]
       , popSender c qs waits, news
       )
  where addReceiver xs p (Receivers qs) = Receivers ((xs,p):qs)
        addReceiver xs p (Senders _) = error "shouldn't happen"
        popSender c [] = rmEntry c
        popSender c qs = fMapUpdate c (const (Senders qs))
doRecv _ _ _ _  = error "type error in recv"

doNu :: MonadFresh m => Name -> Pi -> St -> m St
doNu x p (ps, waits, news) =
  fresh >>= \i ->
  return (substPi [(x, N i)] p : ps, waits, i : news)

-- some tests

startSt :: St
startSt = ([startE], [], [])

{-   (new i . c!i . i?<x,y> . i!(x+y) . end) |
     (c?j . j!<3,4> . j?z . end)
-}

startE =
   Nu i (Send (eN c) (eN i)
     (Recv (eN i) (PT [PN x, PN y])
        (Send (eN i) (EPlus (eN x) (eN y)) End))) `Par`
   Recv (eN c) (PN j)
     (Send (eN j) (ETup [eI 3, eI 4])
       (Recv (eN j) (PN z) End))
  where (i,j,c,x,y,z) = (1,2,3,4,5,6)

iterateM :: Monad m => (a -> m a) -> a -> m [a]
iterateM f x = (f x >>= iterateM f) >>= (return . (x:))

trace :: [St]
trace = fst . flip runState (-1) $
          (iterateM step startSt :: PiMonad [St])
  where fst3 (x,y,z) = x
