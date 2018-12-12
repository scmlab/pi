module Interaction.Util where

import System.IO
import System.Console.ANSI

--------------------------------------------------------------------------------
-- | IO control

-- special mode
controlStdin :: IO ()
controlStdin = do
  hSetBuffering stdin NoBuffering
  hSetEcho      stdin False

-- normal mode
restoreStdin :: IO ()
restoreStdin = do
  hSetBuffering stdin LineBuffering
  hSetEcho      stdin True

-- to capture keys that is longer than 1 character
interceptStdin :: String -> IO String
interceptStdin input = reverse <$> interceptStdin' input
  where
    interceptStdin' buffer = do
      char <- getChar
      more <- hReady stdin
      if more
        then interceptStdin' (char:buffer)
        else return          (char:buffer)


--------------------------------------------------------------------------------
-- | Colouring stuff

yellow :: IO () -> IO ()
yellow p = do
  setSGR [SetColor Foreground Vivid Yellow]
  p
  setSGR []

blue :: IO () -> IO ()
blue p = do
  setSGR [SetColor Foreground Dull Blue]
  p
  setSGR []

red :: IO () -> IO ()
red p = do
  setSGR [SetColor Foreground Vivid Red]
  p
  setSGR []

green :: IO () -> IO ()
green p = do
  setSGR [SetColor Foreground Vivid Green]
  p
  setSGR []
