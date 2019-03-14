module Runtime.Scheduler (execute) where

import Data.Text.Prettyprint.Doc

import Control.Monad.State
import Text.Read (readMaybe)

import Runtime
import Runtime.Util
import Interpreter
import Syntax.Abstract

execute :: RuntimeM ()
execute = do
  run handleInput handleOutput
  skipSilent
  execute

handleInput :: RuntimeM Val
handleInput = do
  raw <- liftIO $ do
    -- requesting for input
    restoreStdin
    raw <- getLine
    controlStdin
    return raw
  -- parsing the input
  let result = VI <$> readMaybe raw
  case result of
    Just val -> return val
    Nothing -> do
      liftIO $ putStrLn $ "cannot parse the input"
      handleInput

handleOutput :: Val -> RuntimeM ()
handleOutput val = liftIO $ do
  putStrLn $ show $ pretty val

skipSilent :: RuntimeM ()
skipSilent = do
  next <- selectedFuture
  case next of
    Success _ EffNoop -> execute
    _ -> return ()
