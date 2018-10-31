{-# LANGUAGE OverloadedStrings #-}

module Interaction.Human where

import Control.Arrow ((***))
import Control.Monad.State hiding (State, state)
import Control.Monad.Except
import Data.List (isPrefixOf)
import System.Console.Haskeline

import Interaction
import Interpreter
import Syntax.Abstract

--------------------------------------------------------------------------------
-- | Interfacing with Humans

humanREPL :: IO ()
humanREPL = runInputT defaultSettings $ do
  (result, _) <- runInteraction initialEnv initialPi loop
  outputStrLn (show result)
  return ()
  where
    liftH = lift . lift

    loop :: InteractionM (InputT IO) ()
    loop = do
      -- print the current states
      outcomes <- gets stateOutcomes
      liftH $ outputStrLn (show $ ppOutcomes outcomes)
      -- get user input
      minput <- liftH $ getInputLine "π > "
      case minput of
        Nothing -> return ()
        Just s -> do
          case parseInput s of
            Just (Run n)   -> do
              try (run n)
              loop
            Just (Feed i v) -> do
              try (feed i v)
              loop
            Just _ -> do
              loop
            Nothing -> do
              loop
        where
          parseInput :: String -> Maybe Request
          parseInput s
            | "run " `isPrefixOf` s = Just $ Run (read $ drop 4 s)
            | "feed " `isPrefixOf` s =
                let [i, v] = (map read $ words $ drop 5 s) :: [Int]
                in Just $ Feed i (VI v)
            | otherwise = Nothing

          try :: InteractionM (InputT IO) () -> InteractionM (InputT IO) ()
          try program = do
            program `catchError` \err ->
              liftH $ outputStrLn err

-- po = (nu w) c!w .
--       w?{ PLUS -> w?<x,y> . w!(x + y) . p0; NEG -> w?x . w!(0 - x) . p0 }
-- p1 = c?j . stdin?x . j!NEG . j!x . j?z . stdout!z . end
-- p2 = c?j . j!PLUS . j!<3,4> . j?z . stdout!z . end
-- main = p0 | p1 | p2
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
        recv channel xs p = Recv channel [(xs,p)]
        send channel v p = Send channel v p
        choices channel xss = Recv channel (map (PL *** id) xss)

        neg v = EMinus (EV (VI 0)) v

        [i,j,c,x,y,z] = map NS ["i","j","c","x","y","z"]

    initialPi :: Pi
    initialPi = Call (NS "p0") `Par` Call (NS "p1") `Par` Call (NS "p2")


-- try in GHCi:
-- start defs startE & trace [0,0] & readInp 0 (VI 10) & trace [0,0,0,0,1,1,0,0,0,0] & ppBState
