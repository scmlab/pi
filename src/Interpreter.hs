-- Shin-Cheng Mu, 2018.

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Interpreter
  ( step, lineup, input, call
  , PID(..), HasPID(..), invokedBy
  , PiMonad, runPiMonad
  , Effect(..), IOTask(..), St(..), Sender(..), Receiver(..), Caller(..), ReplNu(..)
  , initialState
  , module Base
  ) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Arrow ((***))

import qualified Data.Map as Map
import Data.Function (on)
import Data.Text (Text)

import Data.Text.Prettyprint.Doc
import PPrint ()

import Base
import Syntax.Abstract
import Utilities

--------------------------------------------------------------------------------
-- | PID: data type for tracking processes

data PID = PID
  { pReplicable   :: Bool       -- can be infinitely replicated
  , pInvokedBy    :: ProcName   -- the process that invoked this
  , pID           :: Int        -- unique ID
  }
  deriving (Show)

type Attr = Int -> PID -- attributes of PID without the ID part

makeReplicable :: Attr -> Attr
makeReplicable attr i = (attr i) { pReplicable = True }

instance Eq PID where
  (==) = (==) `on` pID

invokedBy :: HasPID a => a -> ProcName
invokedBy = pInvokedBy . getPID

isReplicable :: HasPID a => a -> Bool
isReplicable = pReplicable . getPID

class HasPID a where
  getPID :: a -> PID

instance HasPID Sender where
  getPID (Sender pid _ _ _) = pid

instance HasPID Receiver where
  getPID (Receiver pid _ _) = pid

instance HasPID Caller where
  getPID (Caller pid _) = pid
  -- getPID (Replicater pid _) = pid

instance HasPID ReplNu where
  getPID (ReplNu pid _ _) = pid

instance HasPID IOTask where
  getPID (Input pid _) = pid
  getPID (Output pid _ _) = pid

--------------------------------------------------------------------------------
-- |

data Sender   = Sender   PID Chan Val Proc            deriving (Show)
data Receiver = Receiver PID Chan [Clause]          deriving (Show)
data Caller   = Caller   PID ProcName               deriving (Show)
data ReplNu   = ReplNu   PID Text Proc               deriving (Show)
data IOTask   = Input    PID      [Clause]
              | Output   PID      Val Proc
              deriving (Show)


instance Eq Sender where
  (==) = (==) `on` getPID

instance Eq Receiver where
  (==) = (==) `on` getPID

instance Eq Caller where
  (==) = (==) `on` getPID

instance Eq ReplNu where
  (==) = (==) `on` getPID

instance Eq IOTask where
  (==) = (==) `on` getPID

data St = St
  { stSenders   :: FMap Chan Sender     -- senders
  , stReceivers :: FMap Chan Receiver   -- receivers
  , stCallers   :: [Caller]             -- callers to some processes
  , stReplNus   :: [ReplNu]             -- replicable restrictions
  , stIOTasks   :: [IOTask]             -- I/O tasks
  , stFreshVars :: [Chan]               -- new variables
  , stPIDCount  :: Int
  , stVarCount  :: Int
  } deriving (Show)

initialState :: St
initialState = St [] [] [] [] [] [] 0 0

data Effect = EffNoop                                   -- nothing ever happened
            | EffCall Caller Proc                         -- calling some process
            | EffComm Chan (Sender, Receiver) (Proc, Proc)  -- process communicateion
            | EffReplNu ReplNu Proc
            | EffIO   IOTask
            deriving (Show)

type PiMonad = ReaderT Env (StateT St (EitherT String []))

runPiMonad :: Env -> St -> PiMonad a -> [Either String (a, St)]
runPiMonad env st m = runEitherT (runStateT (runReaderT m env) st)

freshVar :: PiMonad Chan
freshVar = do
  i <- gets stVarCount
  modify $ \st -> st { stVarCount = succ i }
  return (NG (Pos i))

freshPID :: Attr -> PiMonad PID
freshPID attr = do
  i <- gets stPIDCount
  let pid = attr (succ i)
  modify $ \st -> st { stPIDCount = succ i }
  return pid

addSender :: Chan -> Sender -> PiMonad ()
addSender name x = do
  xs <- gets stSenders
  modify $ \st -> st { stSenders = (name, x):xs }

addReceiver :: Chan -> Receiver -> PiMonad ()
addReceiver name x = do
  xs <- gets stReceivers
  modify $ \st -> st { stReceivers = (name, x):xs }

addCaller :: Caller -> PiMonad ()
addCaller x = do
  xs <- gets stCallers
  modify $ \st -> st { stCallers = x:xs }

addReplNu :: ReplNu -> PiMonad ()
addReplNu x = do
  xs <- gets stReplNus
  modify $ \st -> st { stReplNus = x:xs }

addIOTask :: IOTask -> PiMonad ()
addIOTask x = do
  xs <- gets stIOTasks
  modify $ \st -> st { stIOTasks = x:xs }

addFreshVar :: Chan -> PiMonad ()
addFreshVar x = do
  xs <- gets stFreshVars
  modify $ \st -> st { stFreshVars = x:xs }

--------------------------------------------------------------------------------
-- | Proc <-> St
addPi :: Attr -> Proc -> PiMonad ()

addPi _ End = return ()

addPi attr (Par p q) = do
  addPi attr p
  addPi attr q

addPi attr (Repl p) = do
  addPi (makeReplicable attr) p

addPi attr (Call callee) = do
  pid <- freshPID attr
  addCaller (Caller pid callee)

addPi attr (Send (NR StdOut) x p) = do
  pid <- freshPID attr
  val <- evalExpr x
  addIOTask (Output pid val p)

addPi attr (Send c x p) = do
  pid <- freshPID attr
  val <- evalExpr x
  addSender c (Sender pid c val p)

addPi attr (Recv (NR StdIn) clauses) = do
  pid <- freshPID attr
  addIOTask (Input pid clauses)

addPi attr (Recv c clauses) = do
  pid <- freshPID attr
  addReceiver c (Receiver pid c clauses)

addPi attr (Nu x _ p) = do
  pid <- freshPID attr
  let replNu = ReplNu pid x p

  if pReplicable (attr 0)
    then do
      addReplNu replNu
    else do
      restict replNu >>= addPi attr

lineup :: [(ProcName, Proc)] -> PiMonad ()
lineup []     = return ()
lineup ((c, x):xs) = do
  addPi (PID False c) x
  lineup xs

--------------------------------------------------------------------------------
-- |

step :: PiMonad Effect
step = do
  (gets stSenders >>= select >>= doSend)
    `mplus` (gets stCallers >>= select >>= doCall)
    `mplus` (gets stReplNus >>= select >>= doReplNu)
    `mplus` (gets stIOTasks >>= select >>= doIO)
  where
    doSend :: ((Chan, Sender), FMap Chan Sender) -> PiMonad Effect
    doSend ((channel, sender), otherSenders) = do
      receivers <- gets stReceivers
      -- selected a reagent from the lists of receivers
      (receiver, otherReceivers) <- selectByKey channel receivers
      -- communicate!
      (sender', receiver') <- communicate sender receiver
      -- adjust the state accordingly
      unless (isReplicable sender) $ do
        modify $ \st -> st { stSenders = otherSenders }
      unless (isReplicable receiver) $ do
        modify $ \st -> st { stReceivers = otherReceivers }
      -- push the products back to the queue
      lineup
        [ (invokedBy   sender,   sender')
        , (invokedBy receiver, receiver')
        ]
      return (EffComm channel (sender, receiver) (sender', receiver'))

    doIO :: (IOTask, [IOTask]) -> PiMonad Effect
    doIO (task, otherTasks) = do
      unless (isReplicable task) $ do
        modify $ \st -> st { stIOTasks = otherTasks }
      return (EffIO task)

    doCall :: (Caller, [Caller]) -> PiMonad Effect
    doCall (caller, otherCallers) = do
      unless (isReplicable caller) $ do
        modify $ \st -> st { stCallers = otherCallers }
      p <- call caller
      return (EffCall caller p)

    doReplNu :: (ReplNu, [ReplNu]) -> PiMonad Effect
    doReplNu (replNu, _) = do
      p <- restict replNu
      lineup [ (invokedBy replNu, p) ]
      return (EffReplNu replNu p)

restict :: ReplNu -> PiMonad Proc
restict (ReplNu _ x p) = do
  var <- freshVar
  addFreshVar var
  return $ substProc (Map.fromList [(PH x, VC var)]) p

call :: Caller -> PiMonad Proc
call (Caller _ callee) = do
  procs <- asks envProcDefns
  case Map.lookup callee procs of
    Just p  -> do
      addPi (PID False callee) p
      return p
    Nothing -> throwError $ "definition not found (looking for " ++ show (pretty callee) ++ ")"

input :: Val -> IOTask -> PiMonad ()
input val (Input pid pps) =
  case matchClauses pps val of
    Just (th, p) -> lineup [(pInvokedBy pid, substProc th p)]
    Nothing -> throwError "input fails to match"
input _ (Output _ _ _) =
    throwError "expecting Input but got Output"

communicate :: Sender -> Receiver -> PiMonad (Proc, Proc)
communicate (Sender _ _ v q) (Receiver _ _ clauses) =
  case matchClauses clauses v of
    Just (th, p) -> return (q, substProc th p)
    Nothing -> throwError "failed to match sender and receiver"

select :: [a] -> PiMonad (a, [a])
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
    vsep  [ pretty "[Call]   :" <+> pretty caller
          ]
  pretty (EffComm channel (sender, receiver) products) =
    vsep  [ pretty "[Communicate]"
          , pretty "Channel  :" <+> pretty channel
          , pretty "Sender   :" <+> pretty sender
          , pretty "Receiver :" <+> pretty receiver
          , pretty "Products :" <+> pretty products
          ]
  pretty (EffReplNu replNu p) =
    vsep  [ pretty "[Replicate Nu]   :"
          , pretty "ReplNu  :" <+> pretty replNu
          , pretty "Result  :" <+> pretty p
          ]
  pretty (EffIO task) =
    vsep  [ pretty "[I/O]   :" <+> pretty task
          ]

applyRepl :: HasPID a => a -> Proc -> Proc
applyRepl p q = if isReplicable p then Repl q else q

senderToProc :: Sender -> Proc
senderToProc (Sender _ c v p) = Send c (EV v) p

receiverToProc :: Receiver -> Proc
receiverToProc (Receiver _ c clauses) = Recv c clauses

callerToProc :: Caller -> Proc
callerToProc (Caller _ callee) = Call callee

replNuToProc :: ReplNu -> Proc
replNuToProc (ReplNu _ x p) = Repl (Nu x Nothing p)

ioTaskToProc :: IOTask -> Proc
ioTaskToProc (Input _ clauses) = Recv (NR StdIn) clauses
ioTaskToProc (Output _ v p) = Send (NR StdOut) (EV v) p

instance Pretty IOTask where
  pretty p = pretty $ applyRepl p (ioTaskToProc p)

instance Pretty Sender where
  pretty p = pretty $ applyRepl p (senderToProc p)

instance Pretty Receiver where
  pretty p = pretty $ applyRepl p (receiverToProc p)

instance Pretty Caller where
  pretty p = pretty $ applyRepl p (callerToProc p)

instance Pretty ReplNu where
  pretty p = pretty $ replNuToProc p

instance Pretty St where
  pretty (St sends recvs callers replNus io news _ _) =
    vsep  [ pretty "Senders  :"
          , indent 2 (vsep (map (pretty . snd) sends))
          , pretty "Receivers:"
          , indent 2 (vsep (map (pretty . snd) recvs))
          , pretty "Callers:"
          , indent 2 (vsep (map pretty callers))
          , pretty "Replicating Nus:"
          , indent 2 (vsep (map pretty replNus))
          , pretty "I/O:"
          , indent 2 (vsep (map (pretty . ioTaskToProc) io))
          , encloseSep (pretty "New: ") (pretty ".") comma (map pretty news)
          ]
