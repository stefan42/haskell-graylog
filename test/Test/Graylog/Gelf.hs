{-# LANGUAGE OverloadedStrings #-}

module Test.Graylog.Gelf where

import qualified Data.Text           as T
import qualified Data.HashMap.Strict as HMS
import           Data.Aeson          (Object, Value(..),encode, decode)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Graylog.Gelf


tests :: TestTree
tests = testGroup "Test.Graylog.Gelf"
   [ testCase "Convert simple message to JSON" case_convertSimpleMessage
   , testCase "Convert additional attributes to JSON" case_convertAdditionalAttributes
   , testCase "Convert level to JSON" case_convertLevel
   ]

case_convertSimpleMessage :: IO ()
case_convertSimpleMessage = do
    jsonObject <- toJsonObject $ (simpleGelf "myHost" "testMessage")
    checkJsonValue jsonObject "version"       (String "1.1")
    checkJsonValue jsonObject "host"          (String "myHost")
    checkJsonValue jsonObject "short_message" (String "testMessage")

case_convertAdditionalAttributes :: IO ()
case_convertAdditionalAttributes = do
    jsonObject <- toJsonObject (simpleGelf "myHost" "testMessage")
        { _gelfAdditionals  = [ gelfSField "team" "delta-force"
                              , gelfNField "size" 42
                              ] 
        }
    checkJsonValue jsonObject "_team" (String "delta-force")
    checkJsonValue jsonObject "_size" (Number 42)

case_convertLevel :: IO ()
case_convertLevel = mapM_
    (\(level, intValue) -> do
        jsonObject <- toJsonObject (simpleGelf "myHost" "testMessage")
            { _gelfLevel = Just level }
        checkJsonValue jsonObject "level" (Number intValue)
    )
    [ (Emergency     , 0)
    , (Alert         , 1)
    , (Critical      , 2)
    , (Error         , 3)
    , (Warning       , 4)
    , (Notice        , 5)
    , (Informational , 6)
    , (Debug         , 7)
    ]

toJsonObject :: GELF -> IO Object
toJsonObject gelf
  = case mbObject of
        Just o  -> return o
        Nothing -> do
            assertFailure "unable to parse Gelf JSON"
            return undefined
  where
    mbObject = decode $ encode gelf

checkJsonValue :: Object -> T.Text -> Value -> IO ()
checkJsonValue jsonObject keyName expectedValue
  = case (HMS.lookup keyName jsonObject) of
        Just actualValue -> actualValue @?= expectedValue
        Nothing	         -> assertFailure $ "could not find key '" ++ (T.unpack keyName) ++ "' in JSON" 