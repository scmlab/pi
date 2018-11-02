{-# LANGUAGE OverloadedStrings #-}

module Interaction.Human where

import Control.Monad.State hiding (State, state)
import Control.Monad.Except
import Data.List (isPrefixOf)
import Data.ByteString.Lazy (readFile)
import System.Console.Haskeline
import System.Directory (makeAbsolute)

import Interaction
import Syntax.Abstract
import Syntax.Parser (parseByteString)
import Prelude hiding (readFile)

--------------------------------------------------------------------------------
-- | Interfacing with Humans

humanREPL :: IO ()
humanREPL = runInputT defaultSettings $ do
  (result, _) <- runInteraction [] (Call (NS "main")) loop
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
          request <- parseRequest s
          case request of
            Just (Run n)   -> do
              try (run n)
              loop
            Just (Feed i v) -> do
              try (feed i v)
              loop
            Just (Load (Prog prog)) -> do
              load $ map (\(PiDecl name p) -> (name, p)) prog
              loop
            Just _ -> do
              loop
            Nothing -> do
              loop
        where
          parseRequest :: String -> InteractionM (InputT IO) (Maybe Request)
          parseRequest s
            | "run " `isPrefixOf` s = return $ Just $ Run (read $ drop 4 s)
            | "feed " `isPrefixOf` s =
              let [i, v] = (map read $ words $ drop 5 s) :: [Int]
              in return $ Just $ Feed i (VI v)
            | "load " `isPrefixOf` s = do
              filepath <- liftIO $ makeAbsolute (init $ drop 5 s)
              raw <- liftIO $ readFile filepath
              case parseByteString raw of
                Left err -> return $ Just $ Err err
                Right prog -> return $ Just $ Load prog
            | otherwise = return Nothing

          -- loadAndParse :: String ->

          try :: InteractionM (InputT IO) () -> InteractionM (InputT IO) ()
          try program = do
            program `catchError` \err ->
              liftH $ outputStrLn err

-- po = (nu w) c!w .
--       w?{ PLUS -> w?<x,y> . w!(x + y) . p0; NEG -> w?x . w!(0 - x) . p0 }
-- p1 = c?j . stdin?x . j!NEG . j!x . j?z . stdout!z . end
-- p2 = c?j . j!PLUS . j!<3,4> . j?z . stdout!z . end
-- main = p0 | p1 | p2
    -- initialEnv :: Env
    -- initialEnv =
    --   [(NS "p0",
    --          Nu i (send c (eN i)
    --           (choices i
    --             [("PLUS", (recv i (PT [PN x, PN y])
    --                   (send i (EPlus (eN x) (eN y)) (Call (NS "p0"))))),
    --              ("NEG", (recv i (PN x)
    --                  (send i (neg (eN x)) (Call (NS "p0")))))])))
    --        ,(NS "p1",
    --           recv c (PN j)
    --               (send j (eL "PLUS")
    --                 (send j (ETup [eI 3, eI 4])
    --                   (recv j (PN z)
    --                     (Send (NR StdOut) (eN z) End)))))
    --        ,(NS "p2",
    --           recv c (PN j)
    --            (recv (NR StdIn) (PN x)
    --             (send j (eL "NEG")
    --              (send j (eN x)
    --                (recv j (PN z)
    --                  (Send (NR StdOut) (eN z) End))))))
    --        ]
    --   where
    --     recv channel xs p = Recv channel [Clause xs p]
    --     send channel v p = Send channel v p
    --     choices channel xss = Recv channel (map (\(l, p) -> Clause (PL l) p) xss)
    --
    --     neg v = EMinus (EV (VI 0)) v
    --
    --     [i,j,c,x,y,z] = map NS ["i","j","c","x","y","z"]
    --
    -- initialPi :: Pi
    -- initialPi = Call (NS "p0") `Par` Call (NS "p1") `Par` Call (NS "p2")


-- try in GHCi:
-- start defs startE & trace [0,0] & readInp 0 (VI 10) & trace [0,0,0,0,1,1,0,0,0,0] & ppBState
