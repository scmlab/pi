{-# LANGUAGE OverloadedStrings #-}

module Test.Interpreter where

import Data.ByteString.Lazy (ByteString)
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.IO.Class

import Interpreter (PiMonad, hasUnguardedRecursion, initialState, runPiMonad)
import Base (programToEnv)
import Syntax.Parser
import Syntax.Abstract


-- for testing purpose
parseProg :: ByteString -> IO Prog
parseProg src = do
  case parseProgram "" src of
    Left err  -> assertFailure $ show err
    Right val -> return val

parseProc :: ByteString -> IO Pi
parseProc src = do
  case parseProcess src of
    Left err  -> assertFailure $ show err
    Right val -> return val

testWith :: ByteString -> PiMonad a -> IO [a]
testWith source program = do
  prog <- parseProg source
  let results = runPiMonad (programToEnv prog) initialState program
  mapM fromEither results
  where
    fromEither (Left err) = assertFailure $ show err
    fromEither (Right (val, _)) = return val

tests :: TestTree
tests = testGroup "Interpreter"
  [ 
  ]

-- unguardedRecursion :: TestTree
-- unguardedRecursion = testGroup "Unguarded Recursion"
--   [ unguardedRecursion0
--   , unguardedRecursion1
--   , unguardedRecursion2
--   , unguardedRecursion3
--   , unguardedRecursion4
--   ]
--
-- source :: ByteString
-- source = "\
-- \a = end\n\
-- \b = b\n\
-- \c = x!3 . c\n\
-- \d = a | * d\n\
-- \e = a | f\n\
-- \f = a | e\n\
-- \"
--
-- unguardedRecursion0 :: TestTree
-- unguardedRecursion0 = testCase "basic" $ do
--   actual <- testWith source $ do
--     hasUnguardedRecursion ["a"] End
--   let expected = [False]
--   actual @?= expected
--
-- unguardedRecursion1 :: TestTree
-- unguardedRecursion1 = testCase "unguarded call" $ do
--   p <- parseProc "b"
--   actual <- testWith source $ do
--     hasUnguardedRecursion ["b"] p
--   let expected = [True]
--   actual @?= expected
--
-- unguardedRecursion2 :: TestTree
-- unguardedRecursion2 = testCase "guarded call" $ do
--   p <- parseProc "x!3 . c"
--   actual <- testWith source $ do
--     hasUnguardedRecursion ["c"] p
--   let expected = [False]
--   actual @?= expected
--
-- unguardedRecursion3 :: TestTree
-- unguardedRecursion3 = testCase "unguarded replication" $ do
--   p <- parseProc "a | * d"
--   actual <- testWith source $ do
--     hasUnguardedRecursion ["d"] p
--   let expected = [True]
--   actual @?= expected
--
-- unguardedRecursion4 :: TestTree
-- unguardedRecursion4 = testCase "mutual" $ do
--   p <- parseProc "a | e"
--   actual <- testWith source $ do
--     hasUnguardedRecursion ["f"] p
--   let expected = [True]
--   actual @?= expected
