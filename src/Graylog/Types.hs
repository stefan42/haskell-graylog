{-# LANGUAGE LambdaCase      #-}
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

import           Data.List
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

openGraylog
   :: HostName -> ServiceName -> ChunkSize -> IO (Either String Graylog)
openGraylog h p cksize = getAddrInfo Nothing (Just h) (Just p) >>= \case
   []     -> return $ Left "No address info found."
   infos ->
      case find (\i -> addrSocketType i == Datagram) infos of
         Nothing -> return $ Left "No datagram info found for address."
         Just  i -> do
            sock <- socket (addrFamily i) Datagram defaultProtocol
            connect sock (addrAddress i)
            hostname <- getHostName
            return $ Right $ Graylog h p i sock (T.pack hostname) cksize

closeGraylog :: Graylog -> IO ()
closeGraylog Graylog{..} = close _graylogSocket
