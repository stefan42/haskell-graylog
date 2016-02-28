{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Graylog.UDP where

import qualified Data.ByteString    as BS
import           Data.FileEmbed
import qualified Data.Text.Encoding as T
import           Test.Tasty
import           Test.Tasty.HUnit

import           Graylog.UDP

largeSample :: BS.ByteString
largeSample = $(embedFile "./test/Test/Graylog/UDP/large-sample.json")

tests :: TestTree
tests = testGroup "Test.Graylog.UDP"
   [ testCase "Send sample message." case_sendSample
   , testCase "Send large sample message." case_sendLargeSample
   ]

case_sendSample :: IO ()
case_sendSample = do
   eglog <- openGraylog "192.168.99.100" "12201" defaultChunkSize
   case eglog of
      Left  e -> assertFailure e
      Right g -> sendLog g sample >> closeGraylog g
   where
      sample = simpleGelf "localhost" "hello world!"

case_sendLargeSample :: IO ()
case_sendLargeSample = do
   eglog <- openGraylog "192.168.99.100" "12201" defaultChunkSize
   case eglog of
      Left  e -> assertFailure e
      Right g -> sendLog g sample >> closeGraylog g
   where
      sample = (simpleGelf "localhost" "hello world!")
                  { _gelfFullMessage = Just $ T.decodeUtf8 largeSample }

