-- Shin-Cheng Mu, 2018.

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Interpreter where

import Control.Monad.State
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.Except
import Control.Arrow ((***))
import Syntax.Abstract
import Utilities

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util (putDocW)
import PPrint

class Monad m => MonadFresh m where
  fresh :: m Name

instance MonadFresh PiMonad where
  fresh = get >>= \i ->
          put (i+1) >>
          return (NG i)

type BkSt = Int
type Env = FMap Name Pi

data Sender = Sender Val Pi deriving (Show, Eq)
data Receiver = Receiver [(Ptrn,Pi)] deriving (Show, Eq)

data St = St
  { stSenders   :: FMap Name Sender      -- senders
  , stReceivers :: FMap Name Receiver   -- receivers
  , stWaiting   :: [Receiver]           -- blocked at stdin
  , stFreshVars :: [Name]                   -- new variables
  } deriving (Show)

type PiMonad = ReaderT Env (StateT BkSt (ExceptT String []))

data Reaction = Silent St
              | Output St Sender
              | Input  St Receiver
              deriving (Show)

runPiMonad :: Env -> BkSt -> PiMonad a -> [Either String (a, BkSt)]
runPiMonad env bk m = runExceptT (runStateT (runReaderT m env) bk)

--------------------------------------------------------------------------------
-- | Pi <-> St

addPi :: Pi -> St -> PiMonad St
addPi End       st = return st
addPi (Par p q) st = addPi p st >>= addPi q
addPi (Call x) st = do
  defs <- ask
  case lookup x defs of
    Just p  -> addPi p st
    Nothing -> throwError $ "definition not found (looking for " ++ show (pretty x) ++ ")"
addPi (Send c x p) (St sends recvs inps news) = do
  val <- evalExpr x
  return $ St ((c, (Sender val p)):sends) recvs inps news
addPi (Recv (NR StdIn) pps) (St sends recvs inps news) =
  return $ St sends recvs (Receiver pps:inps) news
addPi (Recv c pps) (St sends recvs inps news) =
  return $ St sends ((c,Receiver pps):recvs) inps news
addPi (Nu x p) (St sends recvs inps news) = do
  i <- fresh
  addPi (substPi [(x, N i)] p) (St sends recvs inps (i:news))

lineup :: [Pi] -> St -> PiMonad St
lineup = flip (foldM (flip addPi))

stToPi :: St -> Pi
stToPi (St sends recvs inps news) =
  foldr Nu (foldr par End ss `par`
            foldr par End rs `par`
            foldr par End is ) news
  where
    ss = [ Send c (EV v) p      | (c,(Sender v p)) <- sends ]
    rs = [ Recv c pps           | (c, Receiver pps) <- recvs ]
    is = [ Recv (NR StdIn) pps  | Receiver pps <- inps]

--------------------------------------------------------------------------------
-- |

step :: St -> PiMonad Reaction
step (St sends recvs inps news) =
  (select inps >>= doInput) `mplus`
  (select sends >>= doSend)
  where
    doSend :: ((Name, Sender), FMap Name Sender) -> PiMonad Reaction
    doSend ((NR StdOut, (Sender v p)), sends') =
      return (Output (St sends' recvs inps news) (Sender v p))
    doSend ((c,(Sender v p)), sends') = do
      (pps, recvs') <- selectByKey c recvs
      qs <- comm (Sender v p) pps
      Silent <$> lineup qs (St sends' recvs' inps news)

    doInput :: (Receiver, [Receiver]) -> PiMonad Reaction
    doInput (pps, inps') =
      return (Input (St sends recvs inps' news) pps)

input :: Val -> Receiver -> St -> PiMonad St
input val (Receiver pps) st =
  case matchPPs pps val of
    Just (th, p) -> lineup [substPi th p] st
    Nothing -> throwError "input fails to match"


select :: MonadPlus m => [a] -> m (a,[a])
select [] = mzero
select (x:xs) = return (x,xs) `mplus`
                ((id *** (x:)) <$> select xs)

comm :: MonadPlus m => Sender -> Receiver -> m [Pi]
comm (Sender v q) (Receiver pps) =
  case matchPPs pps v of
   Just (th, p) -> return [q, substPi th p]
   Nothing -> mzero

-- Pretty Printing

ppStPi :: St -> Doc a
ppStPi = pretty . stToPi

ppSt :: St -> Doc a
ppSt (St sends recvs inps news) =
 vsep [pretty "Senders:",
       indent 2 (vsep (map pretty ss)),
       pretty "Receivers:",
       indent 2 (vsep (map pretty rs)),
       encloseSep (pretty "New: ") (pretty ".") comma
          (map pretty news)]
 where ss = [ Send c (EV v) p     | (c, (Sender v p)) <- sends ]
       rs = [ Recv (NR StdIn) pps | (Receiver pps)    <- inps ] ++
            [ Recv c pps          | (c, Receiver pps) <- recvs ]

ppMsg :: Pretty st => Either ErrMsg st -> Doc a
ppMsg (Left msg) = pretty "error:" <+> pretty msg
ppMsg (Right st) = pretty st

ppMsgSt :: Either ErrMsg (St, b) -> Doc a
ppMsgSt (Left msg) = pretty "error:" <+> pretty msg
ppMsgSt (Right (st, _)) = ppSt st

ppReaction :: Reaction -> Doc a
ppReaction (Silent st) = ppSt st
ppReaction (Output st (Sender v p)) =
  vsep [pretty "Output:" <+>
         pretty (Send (NR StdOut) (EV v) p),
        ppSt st]
ppReaction (Input st (Receiver p)) =
  vsep [pretty "Input:" <+>
         pretty (Recv (NR StdIn) p),
        ppSt st]

ppMsgRes :: Either ErrMsg (Reaction, b) -> Doc a
ppMsgRes (Left msg) = pretty "error:" <+> pretty msg
ppMsgRes (Right (res, _)) = ppReaction res

{-
type St = ( [Pi]               -- processes running
          , FMap Name Waiting  -- processes waiting at each channels
          , [Name]             -- generated names
          )

type Waiting = [Sender] -- only senders wait in the queue

-- data Waiting = Senders [Sender]
--              | Receivers [Receiver]
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
          Name -> Receiver -> St -> m (Res St)
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
