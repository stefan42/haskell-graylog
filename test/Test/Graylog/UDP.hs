{-# LANGUAGE OverloadedStrings #-}

module Test.Graylog.UDP where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Graylog.UDP

tests :: TestTree
tests = testGroup "Test.Graylog.UDP"
   {-[ testCase "Send: Sample" case_validateSomething-}
   [ testCase "Send sample message." case_sendSample
   ]

case_sendSample :: IO ()
case_sendSample = do
   eglog <- openGraylog "192.168.99.100" "12201" defaultChunkSize
   case eglog of
      Left  e -> assertFailure e
      Right g -> sendLog g sample
   where
      sample = simpleGelf "localhost" "hello world!"
