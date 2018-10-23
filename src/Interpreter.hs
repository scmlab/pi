-- Shin-Cheng Mu, 2018.

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Interpreter where

import Control.Monad.State
import Control.Monad.Except
import Control.Arrow ((***))
import Syntax
import PiMonad
import Utilities

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util (putDocW)
import PPrint

type Env = FMap Name Pi

type St = ( FMap Name (Val, Pi)     -- senders
          , FMap Name [(Ptrn, Pi)]  -- receivers
          , [[(Ptrn, Pi)]]          -- blocked at stdin
          , [Name])                 -- new variables

stToPi :: St -> Pi
stToPi (sends, recvs, inps, news) =
  foldr Nu (foldr par End ss `par`
            foldr par End rs `par`
            foldr par End is ) news
  where
    ss = [ Send c (EV v) p | (c,(v,p)) <- sends ]
    rs = [ Recv c pps | (c,pps) <- recvs ]
    is = [ Recv (NR StdIn) pps | pps <- inps]

lineup :: (MonadFresh m, MonadError ErrMsg m) =>
    Env -> [Pi] -> St -> m St
lineup _ [] st = return st
lineup defs (End : ps) st = lineup defs ps st
lineup defs (Par p1 p2 : ps) st =
  lineup defs (p1:p2:ps) st
lineup defs (Call x : ps) st
  | Just p <- lookup x defs =
    lineup defs (p:ps) st
  | otherwise = throwError "definition not found"
lineup defs (Send c x p : ps) (sends, recvs, inps, news) =
   evalExpr x >>= \v ->
   lineup defs ps ((c,(v,p)):sends, recvs, inps, news)
lineup defs (Recv (NR StdIn) pps : ps)
            (sends, recvs, inps, news) =
   lineup defs ps (sends, recvs, pps:inps, news)
lineup defs (Recv c pps : ps) (sends, recvs, inps, news) =
   lineup defs ps (sends, (c,pps):recvs, inps, news)
lineup defs (Nu x p : ps) (sends, recvs, inps, news) =
  fresh >>= \i ->
  lineup defs (substPi [(x, N i)] p : ps)
      (sends, recvs, inps, i:news)

select :: MonadPlus m => [a] -> m (a,[a])
select [] = mzero
select (x:xs) = return (x,xs) `mplus`
                ((id *** (x:)) <$> select xs)

data Res = Silent St
         | Output Val Pi St
         | Input [(Ptrn, Pi)] St

step :: (MonadFresh m, MonadError ErrMsg m, MonadPlus m) =>
        Env -> St -> m Res
step defs (sends, recvs, inps, news) =
   (select inps >>= doInput) `mplus`
   (select sends >>= doSend)
 where
   doSend :: (MonadFresh m, MonadError ErrMsg m,
              MonadPlus m) =>
      ((Name, (Val, Pi)), FMap Name (Val,Pi))  -> m Res
   doSend ((NR StdOut, (v,p)), sends') =
     return (Output v p (sends', recvs, inps, news))
   doSend ((c,(v,p)), sends') =
     selectByKey c recvs >>= \(pps, recvs') ->
     comm (v,p) pps >>= \qs ->
     Silent <$> lineup defs qs (sends', recvs', inps, news)
   doInput :: (MonadFresh m, MonadError ErrMsg m,
               MonadPlus m) =>
      ([(Ptrn, Pi)], [[(Ptrn, Pi)]]) -> m Res
   doInput (pps, inps') =
      return (Input pps (sends, recvs, inps', news))

input :: (MonadFresh m, MonadError ErrMsg m) =>
       Env -> Val -> [(Ptrn, Pi)] -> St -> m St
input defs v pps st =
  case matchPPs pps v of
   Just (th, p) -> lineup defs [substPi th p] st
   Nothing -> throwError "input fails to match"

comm :: MonadPlus m => (Val, Pi) -> [(Ptrn, Pi)] -> m [Pi]
comm (v,q) pps =
  case matchPPs pps v of
   Just (th, p) -> return [q,substPi th p]
   Nothing -> mzero

-- Pretty Printing

ppStPi :: St -> Doc a
ppStPi = pretty . stToPi

ppSt :: St -> Doc a
ppSt (sends, recvs, inps, news) =
 vsep [pretty "Senders:",
       indent 2 (vsep (map pretty ss)),
       pretty "Receivers:",
       indent 2 (vsep (map pretty rs)),
       encloseSep (pretty "New: ") (pretty ".") comma
          (map pretty news)]
 where ss = [ Send c (EV v) p | (c,(v,p)) <- sends ]
       rs = [ Recv (NR StdIn) pps | pps <- inps ] ++
            [ Recv c pps | (c,pps) <- recvs ]

ppMsg :: Pretty st => Either ErrMsg st -> Doc a
ppMsg (Left msg) = pretty "error:" <+> pretty msg
ppMsg (Right st) = pretty st

ppMsgSt :: Either ErrMsg (St, b) -> Doc a
ppMsgSt (Left msg) = pretty "error:" <+> pretty msg
ppMsgSt (Right (st, _)) = ppSt st

ppRes :: Res -> Doc a
ppRes (Silent st) = ppSt st
ppRes (Output v p st) =
  vsep [pretty "Output:" <+>
         pretty (Send (NR StdOut) (EV v) p),
        ppSt st]
ppRes (Input p st) =
  vsep [pretty "Input:" <+>
         pretty (Recv (NR StdIn) p),
        ppSt st]

ppMsgRes :: Either ErrMsg (Res, b) -> Doc a
ppMsgRes (Left msg) = pretty "error:" <+> pretty msg
ppMsgRes (Right (res, _)) = ppRes res

{-
type St = ( [Pi]               -- processes running
          , FMap Name Waiting  -- processes waiting at each channels
          , [Name]             -- generated names
          )

type Waiting = [(Val, Pi)] -- only senders wait in the queue

-- data Waiting = Senders [(Val, Pi)]
--              | Receivers [[(Ptrn, Pi)]]
--    deriving (Eq, Show)
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

instance Functor Res where
  fmap f (Silent x) = Silent (f x)
  fmap f (Output v x) = Output (f x) v
  fmap f (Input x) = Input (f x)

step :: (MonadFresh m, MonadError ErrMsg m) =>
        Env -> St -> m (Res St)
step defs ([], waits, news) = mzero
step defs (End : ps, waits, news) =
  step defs (ps, waits, news)
step defs (Par p1 p2 : ps, waits, news) =
  step defs (p1:p2:ps, waits, news)
step defs (Send c x p : ps, waits, news) =
  evalExpr x >>= \v ->
    doSend c v p (ps, waits, news) `mplus`
  fmap (fork3 (Send c x p :) id id)
    step defs (ps, waits, news)
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
  return (Output v (p:ps, waits, news))
doSend c v p (ps, waits, news) =
  step (ps, fMapUpdate c (v,p) ((v,p):) waits, news)

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
  step (substPi [(x, N i)] p : ps, waits, i : news)
-}
