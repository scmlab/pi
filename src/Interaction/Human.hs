{-# LANGUAGE OverloadedStrings #-}

module Interaction.Human where

import Control.Monad.State hiding (State, state)
import Control.Monad.Except
import Data.List (isPrefixOf)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (pretty)
import System.Console.Haskeline
import System.Directory (makeAbsolute)
import System.IO
import Text.Read (readMaybe)

import Interaction
import Interpreter (Reaction(..))
import Syntax.Abstract
import Syntax.Parser (ParseError(..), parseByteString)
import Prelude hiding (readFile)

--------------------------------------------------------------------------------
-- | Interfacing with Humans

humanREPL :: IO ()
humanREPL = runInputT defaultSettings $ do
  (result, _) <- runInteraction loop
  outputStrLn (show result)
  return ()
  where
    liftH = lift . lift

    loop :: InteractionM (InputT IO) ()
    loop = do
      -- print the current states
      gets stateOutcomes >>= liftH . outputStrLn . show . pretty
      -- get user input
      minput <- liftH $ getInputLine "π > "
      case minput of
        Nothing -> return ()
        Just s -> do
          request <- parseRequest s
          case request of
            ReqChoose n -> do
              try (choose n)
              loop
            ReqRun -> do
              try run
              loop
            ReqFeed v -> do
              try (feed v)
              loop
            ReqLoad (Prog prog) -> do
              load $ map (\(PiDecl name p) -> (name, p)) prog
              loop
            ReqParseErr err -> do
              liftH $ outputStrLn (show err)
              loop
            ReqOtherErr err -> do
              liftH $ outputStrLn (show err)
              loop
        where
          parseRequest :: String -> InteractionM (InputT IO) Request
          parseRequest s
            | "run" `isPrefixOf` s = return ReqRun
            | "feed " `isPrefixOf` s = return $ ReqFeed (VI . read $ drop 5 s)
            | "load " `isPrefixOf` s = do
              filepath <- liftIO $ makeAbsolute (init $ drop 5 s)
              raw <- liftIO $ BS.readFile filepath
              case parseByteString raw of
                Left err -> return $ ReqParseErr err
                Right prog -> return $ ReqLoad prog
            | otherwise = case readMaybe s of
                Just n  -> return $ ReqChoose n
                Nothing -> return $ ReqParseErr (RequestParseError "cannot understand your command")

          -- loadAndParse :: String ->

          try :: InteractionM (InputT IO) () -> InteractionM (InputT IO) ()
          try program = do
            program `catchError` \err ->
              liftH $ outputStrLn err


data Key = Up | Down | Next | Other String

newREPL :: FilePath -> IO ()
newREPL filePath = do

  -- start looping
  void $ runInteraction $ do
    -- read file
    parseFile filePath >>= handleRequest

    loop
  where
    loop :: InteractionM IO ()
    loop = do
      getKey >>= keyToRequst . parseKey >>= handleRequest
      loop

    parseKey :: String -> Key
    parseKey key = case key of
      "\ESC[A" -> Up
      "\ESC[B" -> Down
      "\ESC[C" -> Next
      -- "\ESC[D" -> return $ ResParseError $ RequestParseError "←"
      "\n"     -> Next
      -- "\DEL"   -> return $ ResParseError $ RequestParseError "⎋"
      _        -> Other key

    keyToRequst :: Key -> InteractionM IO Request
    keyToRequst Up   = withCursor (return . ReqChoose . (flip (-) 1))
    keyToRequst Down = withCursor (return . ReqChoose . (flip (+) 1))
    keyToRequst Next = do
      outcome <- retrieveOutcome
      case outcome of
        Success _ (Input _) _ -> do
          void $ error "not implemented yet"
          return $ ReqFeed (VI 3)
        Success _ _ _         -> return $ ReqRun
        Failure err           -> return $ ReqOtherErr err
    keyToRequst (Other key) = return $ ReqParseErr $ RequestParseError (Text.pack key)

    handleRequest :: Request -> InteractionM IO ()
    handleRequest (ReqChoose n) = try $ do
      choose n
      outcome <- retrieveOutcome
      liftIO $ putStrLn $ show $ pretty outcome
      printStatusBar
    handleRequest ReqRun = do
      try run
      -- currentState >>= liftIO . putStrLn . show . pretty
      retrieveOutcome >>= liftIO . putStrLn . show . pretty
      printStatusBar
    handleRequest (ReqLoad (Prog prog)) = do
      load $ map (\(PiDecl name p) -> (name, p)) prog
      -- choose the first outcome and print its state
      state <- retrieveNthOutcome 0 >>= toState
      -- liftIO $ putStrLn $ show (length outcomes) ++ " possible outcomes"
      liftIO $ putStrLn $ show $ pretty state
      printStatusBar
    handleRequest (ReqFeed _) = void $ error "not implemented yet"
    handleRequest (ReqParseErr msg) = liftIO $ putStrLn (show msg)
    handleRequest (ReqOtherErr msg) = liftIO $ putStrLn (show msg)

    printStatusBar :: InteractionM IO ()
    printStatusBar = do
      outcomes <- gets stateOutcomes
      cursor <- gets stateCursor
      case cursor of
        Nothing -> liftIO $ putStrLn $ "0/0 outcomes"
        Just n  -> liftIO $ putStrLn $ show (n + 1) ++ "/" ++ show (length outcomes) ++ " outcomes"

    parseFile :: FilePath -> InteractionM IO Request
    parseFile path = do
      rawFile <- liftIO $ BS.readFile path
      case parseByteString rawFile of
        Left err   -> return $ ReqParseErr err
        Right prog -> return $ ReqLoad prog

    getKey :: InteractionM IO String
    getKey = do
      liftIO $ hSetBuffering stdin NoBuffering
      liftIO $ hSetEcho stdin False

      key <- reverse <$> getKey' ""

      liftIO $ hSetBuffering stdin LineBuffering
      liftIO $ hSetEcho stdin True

      return key

      where getKey' chars = do
              char <- liftIO $ getChar
              more <- liftIO $ hReady stdin
              if more
                then getKey' (char:chars)
                else return  (char:chars)

    try :: InteractionM IO () -> InteractionM IO ()
    try program = do
      program `catchError` \_ -> return ()
      -- program `catchError` (liftIO . putStrLn . show . pretty)

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
