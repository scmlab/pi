module Main where

import Control.Arrow ((***))
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except


import Data.Function ((&))
import Data.List (isPrefixOf)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util (putDocW)
import System.Console.Haskeline

import Syntax
import PiMonad
import Interpreter
import Utilities (FMap)
import Backend
import PPrint

main :: IO ()
main = haskeline

haskeline :: IO ()
haskeline = runInputT defaultSettings $ do
  (result, state) <- runInteraction initialEnv initialPi loop
  outputStrLn (show result)
  -- outputStrLn (show result)
  return ()
  where
    liftH = lift . lift . lift

    loop :: InteractionM (InputT IO) ()
    loop = do
      -- print the current environment
      states <- get
      liftH $ outputStrLn (show $ ppStates states)
      -- get user input
      minput <- liftH $ getInputLine "π > "
      case minput of
        Nothing -> return ()
        Just input -> do
          case parseInput input of
            Just (Choose n)   -> do
              try (choose n)
              loop
            Just (Feed i v) -> do
              try (feed i v)
              loop
            Nothing         -> loop
        where
          parseInput :: String -> Maybe Request
          parseInput input
            | "choose " `isPrefixOf` input = Just $ Choose (read $ drop 7 input)
            | "feed " `isPrefixOf` input =
                let [i, v] = (map read $ words $ drop 5 input) :: [Int]
                in Just $ Feed i (VI v)
            | otherwise = Nothing

          try :: InteractionM (InputT IO) () -> InteractionM (InputT IO) ()
          try program = do
            program `catchError` \err ->
              liftH $ outputStrLn err

    initialBState :: BState
    initialBState = start initialEnv initialPi

    initialEnv :: Env
    initialEnv =
      [(NS "p0",
             Nu i (send c (eN i)
              (choices i
                [("PLUS", (recv i (PT [PN x, PN y])
                      (send i (EPlus (eN x) (eN y)) (Call (NS "p0"))))),
                 ("NEG", (recv i (PN x)
                     (send i (neg (eN x)) (Call (NS "p0")))))])))
           ,(NS "p1",
              recv c (PN j)
                  (send j (eL "PLUS")
                    (send j (ETup [eI 3, eI 4])
                      (recv j (PN z)
                        (Send (NR StdOut) (eN z) End)))))
           ,(NS "p2",
              recv c (PN j)
               (recv (NR StdIn) (PN x)
                (send j (eL "NEG")
                 (send j (eN x)
                   (recv j (PN z)
                     (Send (NR StdOut) (eN z) End))))))
           ]
      where
        recv c xs p = Recv c [(xs,p)]
        send c v p = Send c v p
        choices c xss = Recv c (map (PL *** id) xss)

        neg x = EMinus (EV (VI 0)) x

        [i,j,c,x,y,z] = map NS ["i","j","c","x","y","z"]

    initialPi :: Pi
    initialPi = Call (NS "p0") `Par` Call (NS "p1") `Par` Call (NS "p2")


-- try in GHCi:
-- start defs startE & trace [0,0] & readInp 0 (VI 10) & trace [0,0,0,0,1,1,0,0,0,0] & ppBState
