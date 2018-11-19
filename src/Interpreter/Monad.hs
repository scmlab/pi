{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses #-}

module Interpreter.Monad where

import Control.Applicative
import Control.Monad.Except

import Syntax.Abstract
import Control.Monad.State
import Control.Monad.Reader
import Utilities

class Monad m => MonadFresh m where
  fresh :: m Name

instance MonadFresh PiMonad where
  fresh = get >>= \i ->
          put (i+1) >>
          return (NG (Pos i))

type BkSt = Int
type Env = FMap Name Pi

type PiMonad = ReaderT Env (StateT BkSt (EitherT String []))

runPiMonad :: Env -> BkSt -> PiMonad a -> [Either String (a, BkSt)]
runPiMonad env bk m = runEitherT (runStateT (runReaderT m env) bk)

--------------------------------------------------------------------------------
-- | The EitherT monad transformer, from https://hackage.haskell.org/package/either-4.4.1/

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Show (m (Either e a)) => Show (EitherT e m a) where
  showsPrec d (EitherT m) = showParen (d > 10) $
    showString "EitherT " . showsPrec 11 m

instance Read (m (Either e a)) => Read (EitherT e m a) where
  readsPrec d = readParen (d > 10)
    (\r' -> [ (EitherT m, t)
            | ("EitherT", s) <- lex r'
            , (m, t) <- readsPrec 11 s])

eitherT :: Monad m => (a -> m c) -> (b -> m c) ->
           EitherT a m b -> m c
eitherT f g (EitherT m) = m >>= \z -> case z of
  Left a -> f a
  Right b -> g b

instance Monad m => Applicative (EitherT e m) where
  pure a  = EitherT $ return (Right a)
  {-# INLINE pure #-}
  EitherT f <*> EitherT v = EitherT $ f >>= \mf -> case mf of
    Left  e -> return (Left e)
    Right k -> v >>= \mv -> case mv of
      Left  e -> return (Left e)
      Right x -> return (Right (k x))
  {-# INLINE (<*>) #-}

instance Monad m => Functor (EitherT e m) where
  fmap f = EitherT . liftM (fmap f) . runEitherT
  {-# INLINE fmap #-}

instance Monad m => Monad (EitherT e m) where
  return a = EitherT $ return (Right a)
  {-# INLINE return #-}
  m >>= k  = EitherT $ do
    a <- runEitherT m
    case a of
      Left  l -> return (Left l)
      Right r -> runEitherT (k r)
  {-# INLINE (>>=) #-}
  fail = EitherT . fail
  {-# INLINE fail #-}

instance Monad m => MonadError e (EitherT e m) where
  throwError = EitherT . return . Left
  EitherT m `catchError` h = EitherT $ m >>= \a -> case a of
    Left  l -> runEitherT (h l)
    Right r -> return (Right r)

instance (Monad m, Alternative m) =>
         Alternative (EitherT e m) where
  empty = EitherT empty
  EitherT m1 <|> EitherT m2 = EitherT (m1 <|> m2)

instance MonadPlus m => MonadPlus (EitherT e m) where
  mzero = EitherT mzero
  EitherT m1 `mplus` EitherT m2 =
     EitherT (m1 `mplus` m2)
