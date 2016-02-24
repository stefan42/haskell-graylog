module Graylog.UDP
   ( Graylog (..)
   , sendLog

   , module Graylog.Gelf
   ) where

import           Network.Socket

import           Graylog.Gelf

data Graylog
   = Graylog
      { _graylogHost :: String
      , _graylogPort :: String
      }

sendLog :: Graylog -> GELF -> IO ()
sendLog glog msg = undefined
