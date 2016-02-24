module Test.Graylog.UDP where

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Test.Graylog.UDP"
   [ testCase "Validation: Something" case_validateSomething
   ]

case_validateSomething :: IO ()
case_validateSomething = return ()

