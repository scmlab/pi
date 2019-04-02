{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where

import qualified Data.ByteString.Lazy as B
import Test.Tasty
import Test.Tasty.HUnit

import Syntax.Parser
import Syntax.Abstract

tests :: TestTree
tests = testGroup "Parser"
  [
  ]


-- for testing purpose
parseProg :: ByteString -> IO Prog
parseProg src = do
  case parseProgram "" src of
    Left err  -> assertFailure $ show err
    Right val -> return val

parseProc :: ByteString -> IO Pi
parseProc src = do
  case parseProc src of
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

source :: ByteString
source = "\
\a = end\n\
\b = b\n\
\c = x!3 . c\n\
\d = a | * d\n\
\e = a | f\n\
\f = a | e\n\
\"

-- unguardedRecursion1 :: TestTree
-- unguardedRecursion1 = testCase "unguarded call" $ do
--   p <- parseProc "b"
--   actual <- testWith source $ do
--     hasUnguardedRecursion ["b"] p
--   let expected = [True]
--   actual @?= expected


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
--             [ Clause (PtrnVar (NS "v")) End
--             ])
--         , PiDecl (NS "p4") (Nu (NS "x") End)
--         , PiDecl (NS "p5") (Call (NS "p4"))
--         ]
--   actual @?= expected
