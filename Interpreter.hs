-- Shin-Cheng Mu, August 2018.
-- adapted from https://github.com/alexj136/pi/

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Interpreter where

import Control.Monad.State
import Syntax
import PiMonad

type St = ( [Expr]               -- processes running
          , FMap Name Waiting    -- processes waiting at each channels
          , [Name]               -- generated names
          )

stToExpr :: St -> Expr
stToExpr (ps, waits, news) =
  foldr Nu
    ((foldr Par End ps) `Par`
     (foldr Par End (map letWait waits))) news
  where letWait (c, Senders ps) = foldr1 Par [ Send (N c) v p | (v,p) <- ps ]
        letWait (c, Receivers ps) = foldr1 Par [ Recv (N c) x p | (x,p) <- ps ]

data Waiting = Senders [(Val, Expr)] | Receivers [(Name, Expr)]
   deriving Show
       -- non-empty

step :: MonadFresh m => St -> m St
step ([], waits, news) = return ([], waits, news)  -- is this right?
step (End : ps, waits, news) = step (ps, waits, news)
step (Par p1 p2 : ps, waits, news) = step (p1:p2:ps, waits, news)
step (Send c v p : ps, waits, news) = return $ doSend c v p (ps, waits, news)
step (Recv c x p : ps, waits, news) = return $ doRecv c x p (ps, waits, news)
step (Nu x p : ps, waits, news) = doNu x p (ps, waits, news)

doSend :: Val -> Val -> Expr -> St -> St
doSend (N c) v p (ps, waits, news) =
  case lookup c waits of
    Nothing -> (ps, (c, Senders [(v,p)]):waits, news)
    Just (Senders _) -> (ps, fMapUpdate c (addSender v p) waits, news)
    Just (Receivers ((x,q):qs)) ->
            ( [substExp x v q] ++ ps ++ [p]
            , popReceiver c qs waits, news)
  where addSender v p (Senders qs) = Senders ((v,p):qs)
        addSender v p (Receivers _) = error "shouldn't happen"
        popReceiver c [] = rmEntry c
        popReceiver c qs = fMapUpdate c (const (Receivers qs))
-- doSend _ _ _ _  = error "type error in send"

doRecv :: Val -> Name -> Expr -> St -> St
doRecv (N c) x p (ps, waits, news) =
  case lookup c waits of
    Nothing -> (ps, (c, Receivers [(c,p)]):waits, news)
    Just (Receivers _) -> (ps, fMapUpdate c (addReceiver c p) waits, news)
    Just (Senders ((v,q):qs)) ->
       ( [q] ++ ps ++ [substExp x v p]
       , popSender c qs waits, news
       )
  where addReceiver c p (Receivers qs) = Receivers ((c,p):qs)
        addReceiver c p (Senders _) = error "shouldn't happen"
        popSender c [] = rmEntry c
        popSender c qs = fMapUpdate c (const (Senders qs))
-- doRecv _ _ _ _  = error "type error in recv"

doNu :: MonadFresh m => Name -> Expr -> St -> m St
doNu x p (ps, waits, news) =
  fresh >>= \i ->
  return (substExp x (N i) p : ps, waits, i : news)

-- finite map related stuffs

type FMap a b = [(a,b)]

fMapUpdate i f [] = []
fMapUpdate i f ((j,x):xs)
   | i == j    = (i,f x) : xs
   | otherwise = (j,x) : fMapUpdate i f xs

rmEntry i [] = []
rmEntry i ((j,x):xs) | i == j = xs
                     | otherwise = (j,x) : rmEntry i xs


-- some tests

startSt :: St
startSt = ([startE], [], [])

-- startE = Send (N 1) (N 100) (Send (N 1) (N 200) End) `Par`
--          Recv (N 1) 1 (Recv (N 2) 2 End) `Par`
--          Recv (N 1) 3 (Send (N 2) (N 3) End)

startE = Nu 1 (Send (N 2) (N 1) (Send (N 1) (N 100) End)) `Par`
         Recv (N 2) 3 (Recv (N 3) 4 End)

iterateM :: Monad m => (a -> m a) -> a -> m [a]
iterateM f x = (f x >>= iterateM f) >>= (return . (x:))


trace :: [St]
trace = fst . flip runState (-1) $
          (iterateM step startSt :: PiMonad [St])
