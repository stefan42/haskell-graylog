module Graylog.UDP
   ( sendLog

   , module Export
   ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy           as LBS
import           Network.Socket.ByteString.Lazy
import           System.Random.MWC

import           Graylog.Gelf                   as Export
import           Graylog.Types                  as Export

sendLog :: Graylog -> GELF -> IO ()
sendLog glog msg = mapM_ (send $ _graylogSocket glog) cks
   where
      raw = encode msg
      cks = chunky raw

chunky :: LBS.ByteString -> [LBS.ByteString]
chunky = undefined
