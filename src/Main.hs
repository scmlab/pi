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
  case optJSON opts of
    True  -> putStrLn "temporarily unavailable"
    False -> humanREPL filePaths

--------------------------------------------------------------------------------
-- | Command-line arguments

data Options = Options
  { optJSON :: Bool
  } deriving (Show)

defaultOptions :: Options
defaultOptions = Options
  { optJSON = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['j']  ["json"]  (NoArg (\opts -> opts { optJSON = True }))  "talk in JSON format"
  ]

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: pi [OPTION...] filepath"
