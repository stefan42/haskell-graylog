-- | UDP Chunked support for sending messages to graylog.
module Graylog.UDP
   ( sendLog

   , module Export
   ) where

import           Codec.Compression.GZip         (compress)
import           Data.Aeson
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy           as LBS
import           Data.Monoid
import           Data.Word
import           Network.Socket.ByteString.Lazy
import           System.Random

import           Graylog.Gelf                   as Export
import           Graylog.Types                  as Export

sendLog :: Graylog -> GELF -> IO ()
sendLog glog gelfMsg = do
  messageId <- randomIO
  let rawMsg = compress $ encode gelfMsg
      cks    = chunkMessage glog rawMsg
      msgs   = addMessageHeaders messageId cks
  mapM_ (send $ _graylogSocket glog) msgs

chunkMessage :: Graylog -> LBS.ByteString -> [LBS.ByteString]
chunkMessage glog raw = divide raw
  where
    hlen       = 12
    gsize      = (fromIntegral (_graylogChunkSize glog)) - hlen
    divide dat = if LBS.null dat
      then []
      else
        let (pre, post) = LBS.splitAt gsize dat
        in pre : divide post

addMessageHeaders :: Word64 -> [LBS.ByteString] -> [LBS.ByteString]
addMessageHeaders _   []    = []
addMessageHeaders _   [msg] = [msg]
addMessageHeaders mid msgs  =
  if totalNum > 128
    then [] -- message too long, skip message - what else can we do?
    else map addmessageHeader $ zip [0..] msgs
  where
    totalNum                      = length msgs
    magic                         = word8 0x1e <> word8 0x0f
    addmessageHeader (index, msg) = (toLazyByteString
         $ magic
        <> word64BE mid
        <> word8 index
        <> word8 (fromIntegral totalNum)
        <> lazyByteString msg)
