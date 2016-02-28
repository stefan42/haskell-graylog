module Graylog.UDP
   ( sendLog

   , module Export
   ) where

import           Data.Aeson
{-import           Data.ByteString.Builder-}
{-import qualified Data.ByteString.Lazy           as LBS-}
{-import           Data.Word-}
import           Network.Socket.ByteString.Lazy
{-import           System.Random-}

import           Graylog.Gelf                   as Export
import           Graylog.Types                  as Export

sendLog :: Graylog -> GELF -> IO ()
sendLog glog msg = do
   _ <- send (_graylogSocket glog) raw
   print raw
   return ()
   where
      raw = encode msg

{-sendLog :: Graylog -> GELF -> IO ()-}
{-sendLog glog msg = do-}
   {-cks <- chunky glog raw-}
   {-mapM_ (send $ _graylogSocket glog) cks-}
   {-where-}
      {-raw = encode msg-}

{-chunky :: Graylog -> LBS.ByteString -> IO [LBS.ByteString]-}
{-chunky glog raw = do-}
   {-groupId <- randomIO-}
   {-splitAt gsize-}
   {-where-}
      {-magic           = word8 0x1e <> word8 0x0f-}
      {-seqNum          = undefined-}
      {-(count, excess) = quotRem (LBS.length raw) gzie-}
      {-hlen            = 12-}
      {-gsize           = (fromIntegral (_graylogChunkSize glog)) - hlen-}
