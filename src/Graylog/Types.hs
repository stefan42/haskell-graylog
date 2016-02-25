{-# LANGUAGE RecordWildCards #-}

module Graylog.Types
   ( Graylog
   , _graylogHost
   , _graylogPort
   , _graylogAddress
   , _graylogSocket
   , _graylogHostName
   , ChunkSize

   , defaultChunkSize
   , openGraylog
   , closeGraylog
   ) where

import           Data.Text      (Text)
import qualified Data.Text      as T
import           Network.BSD
import           Network.Socket

type ChunkSize = Word

data Graylog
   = Graylog
      { _graylogHost      :: String
      , _graylogPort      :: String
      , _graylogAddress   :: AddrInfo
      , _graylogSocket    :: Socket
      , _graylogHostName  :: Text
      , _graylogChunkSize :: ChunkSize
      }

defaultChunkSize :: ChunkSize
defaultChunkSize = 8192

openGraylog :: HostName -> ServiceName -> ChunkSize -> IO (Either String Graylog)
openGraylog host port chksize = do
   infos <- getAddrInfo Nothing (Just host) (Just port)
   case infos of
      []     -> return $ Left "No address info found."
      [info] -> do
         sock <- socket (addrFamily info) Datagram defaultProtocol
         connect sock (addrAddress info)
         hostname <- getHostName
         return $ Right $ Graylog host port info sock (T.pack hostname) chksize
      _      -> return $ Left "Too many address infos found."

closeGraylog :: Graylog -> IO ()
closeGraylog Graylog{..} = close _graylogSocket


