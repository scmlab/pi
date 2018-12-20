{-# LANGUAGE OverloadedStrings #-}

module Test.TypeChecker where

import Control.Arrow ((***))
import Control.Monad.Except
import Data.Text (Text, pack)
import Test.Tasty
import Test.Tasty.HUnit

import Syntax.Abstract
import Type.TypeCheck
import Type

tests :: TestTree
tests = testGroup "TypeChecker"
  [ testCase "p0" $ do
      let expected = Right ()
      let actual = runExcept $ checkPi [] [] p0
      actual @?= Left "variable Neg \"c\" not found"
  , testCase "p1" $ do
      let expected = Right ()
      let actual = runExcept $ checkPi [] [] p1
      actual @?= Left "variable Pos \"c\" not found"
  , testCase "p2" $ do
      let expected = Right ()
      let actual = runExcept $ checkPi [] [] p2
      actual @?= expected
  ]
  where
    t0 :: SType
    t0 = tsele [("NEG", tsend TInt  $ trecv TInt  $ TEnd),
                ("ID",  tsend TBool $ trecv TBool $ TEnd)]

    t1 :: SType
    t1 = tRecv t0 TEnd

    senv0 :: SEnv
    senv0 = [(Pos (pack "c"), t1),
             (Neg (pack "c"), dual t1),
             (Pos (pack "d"), t0),
             (Neg (pack "d"), dual t0)
             ]


    p0 :: Pi
    p0 = Send (cN "c") (ePN "d") $
          choices (cN "d")
            [("NEG", recv (cN "d") (pn "x") $
                       Send (cN "d") (eI 0 `ESub` ePN "x") End),
             ("ID", recv (cN "d") (pn "x") $
                        Send (cN "d") (ePN "x") End)]

    p1 :: Pi
    p1 = recv (cP "c") (pn "z") $
          Send (cP "z") (eL "NEG") $
           Send (cP "z") (eI 3) $
               recv (cP "z") (pn "w") End

    p2 :: Pi
    p2 = nu "c" t1 (nu "d" t0 p0 `Par` p1)
