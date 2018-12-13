-- Shin-Cheng Mu, 2018.

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Interpreter
  ( step, lineup, input, call
  , PID(..), HasPID(..), invoker
  , PM(..), runPM
  , Effect(..), IOTask(..), St(..), Sender(..), Receiver(..), Caller(..)
  , module Interpreter.Monad
  , senderToPi, receiverToPi, ioTaskToPi, callerToPi
  ) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Arrow ((***))

import qualified Data.Map as Map
import Data.Function (on)

import Data.Text.Prettyprint.Doc
import PPrint ()

import Interpreter.Monad
import Syntax.Abstract
import Utilities

--------------------------------------------------------------------------------
-- | PID: data type for tracking processes

data PID = PID
  Int       -- unique ID
  ProcName  -- the process that invoked this
  deriving (Show)

instance Eq PID where
  PID i _ == PID j _ = i == j

invoker :: HasPID a => a -> ProcName
invoker x = let PID _ name = getPID x in name

class HasPID a where
  getPID :: a -> PID

instance HasPID Sender where
  getPID (Sender pid _ _ _) = pid

instance HasPID Receiver where
  getPID (Receiver pid _ _) = pid

instance HasPID Caller where
  getPID (Caller pid _) = pid
  getPID (Replicater pid _) = pid

instance HasPID IOTask where
  getPID (Input pid _) = pid
  getPID (Output pid _ _) = pid

--------------------------------------------------------------------------------
-- |

data Sender   = Sender   PID Name Val Pi deriving (Show)
data Receiver = Receiver PID Name [Clause] deriving (Show)
data Caller   = Caller   PID ProcName
              | Replicater PID Pi
              deriving (Show)
data IOTask   = Input PID [Clause]
              | Output PID Val Pi
              deriving (Show)

instance Eq Sender where
  (==) = (==) `on` getPID

instance Eq Receiver where
  (==) = (==) `on` getPID

instance Eq Caller where
  (==) = (==) `on` getPID

instance Eq IOTask where
  (==) = (==) `on` getPID

data St = St
  { stSenders   :: FMap Name Sender     -- senders
  , stReceivers :: FMap Name Receiver   -- receivers
  , stCallers   :: [Caller]             -- callers to some processes
  , stIOTasks   :: [IOTask]             -- I/O tasks
  , stFreshVars :: [Name]               -- new variables
  , stPIDCount  :: Int
  , stVarCount  :: Int
  } deriving (Show)

data Effect = EffNoop                                   -- nothing ever happened
            | EffCall Caller Pi                         -- calling some process
            | EffComm Name (Sender, Receiver) (Pi, Pi)  -- some chemical communicateion
            | EffIO   IOTask
            deriving (Show)


type PM = ReaderT Env (StateT St (EitherT String []))

instance MonadFresh PM where
  fresh = do
    i <- gets stVarCount
    modify $ \st -> st { stVarCount = succ i }
    return (NG (Pos i))

runPM :: Env -> St -> PM a -> [Either String (a, St)]
runPM env st m = runEitherT (runStateT (runReaderT m env) st)

freshPID :: ProcName -> PM PID
freshPID name = do
  i <- gets stPIDCount
  let pid = PID (succ i) name
  modify $ \st -> st { stPIDCount = succ i }
  return pid

addSender :: Name -> Sender -> PM ()
addSender name x = do
  xs <- gets stSenders
  modify $ \st -> st { stSenders = (name, x):xs }

addReceiver :: Name -> Receiver -> PM ()
addReceiver name x = do
  xs <- gets stReceivers
  modify $ \st -> st { stReceivers = (name, x):xs }

addCaller :: Caller -> PM ()
addCaller x = do
  xs <- gets stCallers
  modify $ \st -> st { stCallers = x:xs }

addIOTask :: IOTask -> PM ()
addIOTask x = do
  xs <- gets stIOTasks
  modify $ \st -> st { stIOTasks = x:xs }

addFreshVar :: Name -> PM ()
addFreshVar x = do
  xs <- gets stFreshVars
  modify $ \st -> st { stFreshVars = x:xs }

--------------------------------------------------------------------------------
-- | Pi <-> St
addPi :: ProcName -> Pi -> PM ()
addPi _    End       = return ()
addPi name (Par p q) = do
  addPi name p
  addPi name q
addPi name (Repl p) = do
  pid <- freshPID name
  addCaller (Replicater pid p)
addPi name (Call callee) = do
  pid <- freshPID name
  addCaller (Caller pid callee)
addPi name (Send (NR StdOut) x p) = do
  pid <- freshPID name
  val <- evalExpr x
  addIOTask (Output pid val p)
addPi name (Send c x p) = do
  pid <- freshPID name
  val <- evalExpr x
  addSender c (Sender pid c val p)
addPi name (Recv (NR StdIn) clauses) = do
  pid <- freshPID name
  addIOTask (Input pid clauses)
addPi name (Recv c clauses) = do
  pid <- freshPID name
  addReceiver c (Receiver pid c clauses)
addPi name (Nu x _ p) = do
  var <- fresh
  addFreshVar var
  addPi name (substPi [(PH x, N var)] p)

lineup :: [(ProcName, Pi)] -> PM ()
lineup []     = return ()
lineup ((c, x):xs) = do
  addPi c x
  lineup xs

senderToPi :: Sender -> Pi
senderToPi (Sender _ c v p) = Send c (EV v) p

receiverToPi :: Receiver -> Pi
receiverToPi (Receiver _ c clauses) = Recv c clauses

callerToPi :: Caller -> Pi
callerToPi (Caller _ callee) = Call callee
callerToPi (Replicater _ p) = Repl p

ioTaskToPi :: IOTask -> Pi
ioTaskToPi (Input _ clauses) = Recv (NR StdIn) clauses
ioTaskToPi (Output _ v p) = Send (NR StdOut) (EV v) p

--------------------------------------------------------------------------------
-- |

step :: PM Effect
step = do
  io        <- gets stIOTasks
  senders   <- gets stSenders
  receivers <- gets stReceivers
  callers   <- gets stCallers
  (select io >>= doIO) `mplus` (select senders >>= doSend receivers) `mplus` (select callers >>= doCall)
  where
    doSend :: FMap Name Receiver -> ((Name, Sender), FMap Name Sender) -> PM Effect
    doSend receivers ((channel, sender), otherSenders) = do
      -- selected a reagent from the lists of receivers
      (receiver, otherReceivers) <- selectByKey channel receivers
      -- communicate!
      (sender', receiver') <- communicate sender receiver
      -- adjust the state accordingly
      st <- lineup
        [ (invoker   sender,   sender')
        , (invoker receiver, receiver')
        ]
      return (EffComm channel (sender, receiver) (sender', receiver'))

    doIO :: (IOTask, [IOTask]) -> PM Effect
    doIO (task, otherTasks) = do
      return (EffIO task)

    doCall :: (Caller, [Caller]) -> PM Effect
    doCall (caller, otherCallers) = do
      p <- call caller
      return (EffCall caller p)

--
-- step :: St -> PiMonad (St, Effect)
-- step (St sends recvs callers io news i v) = do
--   (select io >>= doIO) `mplus` (select sends >>= doSend) `mplus` (select callers >>= doEffCall)
--   where
--     doSend :: ((Name, Sender), FMap Name Sender) -> PiMonad (St, Effect)
--     -- doSend ((NR StdOut, sender), otherSenders) =
--     --   return (St otherSenders recvs callers io news i, EffIO sender)
--     doSend ((channel, sender), otherSenders) = do
--       -- selected a reagent from the lists of receivers
--       (receiver, otherReceivers) <- selectByKey channel recvs
--       -- communicate!
--       (sender', receiver') <- communicate sender receiver
--       -- adjust the state accordingly
--       st <- lineup
--         [ (invoker   sender,   sender'  )
--         , (invoker receiver, receiver')
--         ]
--         (St otherSenders otherReceivers callers io news i v)
--       return (st, EffComm channel (sender, receiver) (sender', receiver'))
--
--     doIO :: (IOTask, [IOTask]) -> PiMonad (St, Effect)
--     doIO (task, otherTasks) =
--       return (St sends recvs callers otherTasks news i v, EffIO task)
--
--     doEffCall :: (Caller, [Caller]) -> PiMonad (St, Effect)
--     doEffCall (caller, otherCallers) = do
--       (state', p) <- call caller (St sends recvs otherCallers io news i v)
--       return (state', EffCall caller p)

call :: Caller -> PM Pi
call (Caller _ callee) = do
  env <- ask
  case Map.lookup (ND (Pos callee)) env of
    Just p  -> do
      addPi callee p
      return p
    Nothing -> throwError $ "definition not found (looking for " ++ show (pretty callee) ++ ")"
call (Replicater (PID _ name) p) = do
  addPi name p
  addPi name (Repl p)
  return (Par (Repl p) p)

input :: Val -> IOTask -> PM ()
input val (Input (PID _ n) pps) =
  case matchClauses pps val of
    Just (th, p) -> lineup [(n, substPi th p)]
    Nothing -> throwError "input fails to match"
input _ (Output _ _ _) =
    throwError "expecting Input but got Output"

communicate :: Sender -> Receiver -> PM (Pi, Pi)
communicate (Sender _ _ v q) (Receiver _ _ clauses) =
  case matchClauses clauses v of
    Just (th, p) -> return (q, substPi th p)
    Nothing -> throwError "failed to match sender and receiver"

select :: [a] -> PM (a, [a])
select []     = mzero
select (x:xs) = return (x, xs) `mplus`
                ((id *** (x:)) <$> select xs)

--------------------------------------------------------------------------------
-- | Pretty printing

instance Pretty Effect where
  pretty EffNoop =
    vsep  [ pretty "[No-op]"
          ]
  pretty (EffCall caller _) =
    vsep  [ pretty "[Call]   :" <+> pretty (callerToPi caller)
          ]
  pretty (EffComm channel (sender, receiver) products) =
    vsep  [ pretty "[Communicate]"
          , pretty "Channel  :" <+> pretty channel
          , pretty "Sender   :" <+> pretty (senderToPi   sender)
          , pretty "Receiver :" <+> pretty (receiverToPi receiver)
          , pretty "Products :" <+> pretty products
          ]
  pretty (EffIO task) =
    vsep  [ pretty "[I/O]   :" <+> pretty task
          ]

instance Pretty IOTask where
  pretty = pretty . ioTaskToPi

instance Pretty Sender where
  pretty = pretty . senderToPi

instance Pretty Receiver where
  pretty = pretty . receiverToPi

instance Pretty Caller where
  pretty = pretty . callerToPi

instance Pretty St where
  pretty (St sends recvs callers io news _ _) =
    vsep  [ pretty "Senders  :"
          , indent 2 (vsep (map (pretty . senderToPi . snd) sends))
          , pretty "Receivers:"
          , indent 2 (vsep (map (pretty . receiverToPi . snd) recvs))
          , pretty "Callers:"
          , indent 2 (vsep (map (pretty . callerToPi) callers))
          , pretty "I/O:"
          , indent 2 (vsep (map (pretty . ioTaskToPi) io))
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
  fmap f (EffNoop x) = EffNoop (f x)
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
      return . EffNoop $ (ps, (c, Receivers [pps]):waits, news)
    Just (Receivers _) ->
      return . EffNoop $ (ps, fMapUpdate c (addReceiver pps) waits, news)
    Just (Senders ((v,q):qs)) ->
       case matchClauses pps v of
        Just (th, p) ->
          return . EffNoop$ ( [q] ++ ps ++ [substPi th p]
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
