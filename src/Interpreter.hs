-- Shin-Cheng Mu, 2018.

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Interpreter
  ( step, lineup, input
  , PID(..), senderProcName, receiverProcName, callerProcName
  , Reaction(..), St(..), Sender(..), Receiver(..), Caller(..)
  , module Interpreter.Monad
  , senderToPi, receiverToPi, inputToPi, callerToPi
  ) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Arrow ((***))

import qualified Data.Map as Map

import Data.Text.Prettyprint.Doc
import PPrint ()

import Interpreter.Monad
import Syntax.Abstract
import Utilities

--------------------------------------------------------------------------------
-- | PID: data type for tracking processes

data PID = PID Int ProcName deriving (Show)

instance Eq PID where
  PID i _ == PID j _ = i == j

senderProcName :: Sender -> ProcName
senderProcName (Sender (PID _ n) _ _) = n

receiverProcName :: Receiver -> ProcName
receiverProcName (Receiver (PID _ n) _) = n

callerProcName :: Caller -> ProcName
callerProcName (Caller (PID _ n) _) = n

--------------------------------------------------------------------------------
-- |

data Sender   = Sender   PID Val Pi deriving (Show)
data Receiver = Receiver PID [Clause] deriving (Show)
data Caller   = Caller   PID ProcName deriving (Show)

instance Eq Sender where
  Sender i _ _ == Sender j _ _ = i == j

instance Eq Receiver where
  Receiver i _ == Receiver j _ = i == j

instance Eq Caller where
  Caller i _ == Caller j _ = i == j

data St = St
  { stSenders   :: FMap Name Sender     -- senders
  , stReceivers :: FMap Name Receiver   -- receivers
  , stCallers   :: [Caller]             -- callers to some processes
  , stWaiting   :: [Receiver]           -- blocked at stdin
  , stFreshVars :: [Name]               -- new variables
  , stIDCount   :: Int
  } deriving (Show)

data Reaction = Silent                       -- nothing ever happened
              | React  Name (Sender, Receiver) (Pi, Pi)  -- some chemical reaction
              | Output Sender                -- stdout
              | Input  Receiver              -- stdin
              deriving (Show)

--------------------------------------------------------------------------------
-- | Pi <-> St

addPi :: ProcName -> Pi -> St -> PiMonad St
addPi _    End       st = return st
addPi name (Par p q) st = addPi name p st >>= addPi name q
addPi name    (Call callee) (St sends recvs callers inps news i) = do
  let i' = succ i
  let callers' = (Caller (PID i' name) callee):callers
  return $ St sends recvs callers' inps news i
  -- env <- ask
  -- case Map.lookup (ND (Pos x)) env of
  --   Just p  -> addPi x p st
  --   Nothing -> throwError $ "definition not found (looking for " ++ show (pretty x) ++ ")"
addPi name (Send c x p) (St sends recvs callers inps news i) = do
  let i' = succ i
  val <- evalExpr x
  return $ St ((c, (Sender (PID i' name) val p)):sends) recvs callers inps news i'
addPi name (Recv (NR StdIn) pps) (St sends recvs callers inps news i) = do
  let i' = succ i
  return $ St sends recvs callers (Receiver (PID i' name) pps:inps) news i'
addPi name (Recv c pps) (St sends recvs callers inps news i) = do
  let i' = succ i
  return $ St sends ((c,Receiver (PID i' name) pps):recvs) callers inps news i'
addPi name (Nu x _ p) (St sends recvs callers inps news i) = do
  var <- fresh
  addPi name (substPi [(PH x, N var)] p) (St sends recvs callers inps (var:news) i)

lineup :: [(ProcName, Pi)] -> St -> PiMonad St
lineup = flip (foldM (flip (uncurry addPi)))

senderToPi :: (Name, Sender) -> Pi
senderToPi (c, (Sender _ v p)) = Send c (EV v) p

receiverToPi :: (Name, Receiver) -> Pi
receiverToPi (c, (Receiver _ clauses)) = Recv c clauses

callerToPi :: Caller -> Pi
callerToPi (Caller _ callee) = Call callee

inputToPi :: Receiver -> Pi
inputToPi (Receiver _ clauses) = Recv (NR StdIn) clauses
-- stToPi :: St -> Pi
-- stToPi (St sends recvs inps news) =
--   foldr Nu (foldr par End ss `par`
--             foldr par End rs `par`
--             foldr par End is ) news
--   where
--     ss = [ Send c (EV v) p      | (c,(Sender v p)) <- sends ]
--     rs = [ Recv c pps           | (c, Receiver pps) <- recvs ]
--     is = [ Recv (NR StdIn) pps  | Receiver pps <- inps]

--------------------------------------------------------------------------------
-- |

step :: St -> PiMonad (St, Reaction)
step (St sends recvs callers inps news i) = do
  (select inps >>= doInput) `mplus` (select sends >>= doSend)
  where
    doSend :: ((Name, Sender), FMap Name Sender) -> PiMonad (St, Reaction)
    doSend ((NR StdOut, sender), otherSenders) =
      return (St otherSenders recvs callers inps news i, Output sender)
    doSend ((channel, sender), otherSenders) = do
      -- selected a reagent from the lists of receivers
      (receiver, otherReceivers) <- selectByKey channel recvs
      -- react!
      (sender', receiver') <- react sender receiver
      -- adjust the state accordingly
      st <- lineup
        [ (senderProcName   sender,   sender'  )
        , (receiverProcName receiver, receiver')
        ]
        (St otherSenders otherReceivers callers inps news i)
      return (st, React channel (sender, receiver) (sender', receiver'))

    doInput :: (Receiver, [Receiver]) -> PiMonad (St, Reaction)
    doInput (blocked, otherBlocked) =
      return (St sends recvs callers otherBlocked news i, Input blocked)

input :: Val -> Receiver -> St -> PiMonad St
input val (Receiver (PID _ n) pps) st =
  case matchClauses pps val of
    Just (th, p) -> lineup [(n, substPi th p)] st
    Nothing -> throwError "input fails to match"


select :: [a] -> PiMonad (a, [a])
select []     = mzero
select (x:xs) = return (x, xs) `mplus`
                ((id *** (x:)) <$> select xs)

react :: Sender -> Receiver -> PiMonad (Pi, Pi)
react (Sender _ v q) (Receiver _ clauses) =
  case matchClauses clauses v of
    Just (th, p) -> return (q, substPi th p)
    Nothing -> throwError "failed to match sender and receiver"

--------------------------------------------------------------------------------
-- | Pretty printing

instance Pretty Reaction where
  pretty Silent =
    vsep  [ pretty "[Silent]"
          ]
  pretty (React channel (sender, receiver) products) =
    vsep  [ pretty "[React]"
          , pretty "Channel  :" <+> pretty channel
          , pretty "Sender   :" <+> pretty (senderToPi   (channel, sender))
          , pretty "Receiver :" <+> pretty (receiverToPi (channel, receiver))
          , pretty "Products :" <+> pretty products
          ]
  pretty (Output (Sender _ v p)) =
    vsep  [ pretty "[Output]   :" <+> pretty (Send (NR StdOut) (EV v) p)
          ]
  pretty (Input (Receiver _ clauses)) =
    vsep  [ pretty "[Input]    :" <+> pretty (Recv (NR StdIn) clauses)
          ]

instance Pretty St where
  pretty (St sends recvs callers inps news _) =
    vsep  [ pretty "Senders  :"
          , indent 2 (vsep (map (pretty . senderToPi) sends))
          , pretty "Receivers:"
          , indent 2 (vsep (map (pretty . receiverToPi) recvs))
          , pretty "Callers:"
          , indent 2 (vsep (map (pretty . callerToPi) callers))
          , pretty "Inputs:"
          , indent 2 (vsep (map (pretty . inputToPi) inps))
          , encloseSep (pretty "New: ") (pretty ".") comma (map pretty news)
          ]
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
       case matchClauses pps v of
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
