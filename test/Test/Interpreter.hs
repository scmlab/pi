{-# LANGUAGE OverloadedStrings #-}

module Test.Interpreter where

import Data.ByteString.Lazy (ByteString)
import Test.Tasty
import Test.Tasty.HUnit

import Interpreter (PiMonad, hasUnguardedRecursion, initialState, runPiMonad)
import Interpreter.Monad (programToEnv)
import Syntax.Parser (parseByteStringOrRaise)
import Syntax.Abstract


testWith :: ByteString -> PiMonad a -> IO [a]
testWith source program = do
  prog <- parseByteStringOrRaise source
  let results = runPiMonad (programToEnv prog) initialState program
  mapM fromEither results
  where
    fromEither (Left err) = assertFailure $ show err
    fromEither (Right (val, _)) = return val


tests :: TestTree
tests = testGroup "Interpreter"
  [ unguardedRecursion
  ]

unguardedRecursion :: TestTree
unguardedRecursion = testGroup "Unguarded Recursion"
  [ unguardedRecursion0
  , unguardedRecursion1
  ]

unguardedRecursion0 :: TestTree
unguardedRecursion0 = testCase "basic" $ do
  actual <- testWith "a = end" $ do
    hasUnguardedRecursion ["a"] End
  let expected = [False]
  actual @?= expected

unguardedRecursion1 :: TestTree
unguardedRecursion1 = testCase "basic" $ do
  actual <- testWith "a = a" $ do
    hasUnguardedRecursion ["a"] (Call "a")
  let expected = [True]
  actual @?= expected
--
-- basic :: TestTree
-- basic = testCase "basic" $ do
--   raw <- B.readFile "test/Parser/basic.pi"
--   let actual = parseByteString raw
--   let expected = Right $ Prog
--         [ PiDecl (NS "p0") End
--         , PiDecl (NS "p1") (Par End End)
--         , PiDecl (NS "p2") (Send (NS "x") (EV (VI 3)) End)
--         , PiDecl (NS "p3") (Recv (NS "x")
--             [ Clause (PN (NS "v")) End
--             ])
--         , PiDecl (NS "p4") (Nu (NS "x") End)
--         , PiDecl (NS "p5") (Call (NS "p4"))
--         ]
--   actual @?= expected
