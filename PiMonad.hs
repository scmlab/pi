{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module PiMonad where

import Syntax
import Control.Monad.State.Lazy

class Monad m => MonadFresh m where
  fresh :: m Name

instance MonadState Int m => MonadFresh m where
  fresh = get >>= \i ->
          put (i-1) >>
          return i

-- one implementation

type PiMonad = State Int -- StateT Int []
