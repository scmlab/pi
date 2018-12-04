-- {-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.GetOpt
import System.Environment

import Interaction.Human (humanREPL)
-- import Interaction.JSON (jsonREPL)

import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  (opts, filePaths) <- getArgs >>= parseOpts
  case optMode opts of
    Trace -> humanREPL True filePaths
    Execute -> humanREPL False filePaths
    Help -> putStrLn $ usageInfo usage options
    
--------------------------------------------------------------------------------
-- | Command-line arguments

data Mode = Trace | Execute | Help

data Options = Options
  { optMode :: Mode
  }

defaultOptions :: Options
defaultOptions = Options
  { optMode = Execute
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h']  ["help"]  (NoArg (\opts -> opts { optMode = Help }))  "print this help message"
  , Option ['t']  ["trace"]  (NoArg (\opts -> opts { optMode = Trace }))  "trace mode"
  ]

usage :: String
usage =  "Usage: pi [OPTION...] filepath\n"

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo usage options
