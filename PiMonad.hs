{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module PiMonad where

import Syntax
import Control.Monad.Except
import Control.Monad.State.Lazy

class Monad m => MonadFresh m where
  fresh :: m Name

instance (Monad m, MonadState Int m) => MonadFresh m where
  fresh = get >>= \i ->
          put (i-1) >>
          return (NG i)

-- one implementation

type PiMonad = StateT Int (Either String) -- StateT Int []
