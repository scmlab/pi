module Test where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Test.Parser as Parser
import qualified Test.Interpreter as Interpreter
import qualified Test.TypeChecker as TypeChecker

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ Interpreter.tests
    , TypeChecker.tests
    ]
  -- [ testCase "2+2=4" $
  --     2+2 @?= 4
  -- , testCase "7 is even" $
  --     assertBool "Oops, 7 is odd" (even 7)
  -- ]
