module Interaction.Scheduler (execute) where

import Data.Text.Prettyprint.Doc

import Control.Monad.State
import Text.Read (readMaybe)

import Interaction
import Interaction.Util
import Interpreter
import Syntax.Abstract

execute :: InteractionM IO ()
execute = do
  run handleInput handleOutput
  skipSilent
  execute

handleInput :: InteractionM IO Val
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

handleOutput :: Val -> InteractionM IO ()
handleOutput val = liftIO $ do
  putStrLn $ show $ pretty val

skipSilent :: InteractionM IO ()
skipSilent = do
  next <- selectedFuture
  case next of
    Success _ EffNoop _ -> execute
    _ -> return ()
