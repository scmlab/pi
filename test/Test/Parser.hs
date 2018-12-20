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
