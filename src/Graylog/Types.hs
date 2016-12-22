{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Graylog.Types
   ( Graylog
   , _graylogHost
   , _graylogPort
   , _graylogAddress
   , _graylogSocket
   , _graylogHostName
   , _graylogChunkSize
   , _graylogCompession
   , ChunkSize
   , CompressionType(..)

   , defaultChunkSize
   , openGraylog
   , closeGraylog
   ) where

import           Data.List
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Network.BSD
import           Network.Socket

-- | The maximum size of each datagram when using UDP transit methods.
type ChunkSize = Word

data CompressionType
  = PlainText
  | GZIP
  | ZLIB
  deriving (Eq)

-- | Handle for a socket connected to Graylog. In some cases this socket
-- is UDP and will not have a maintained session.
data Graylog
   = Graylog
      { _graylogHost       :: String
      , _graylogPort       :: String
      , _graylogAddress    :: AddrInfo
      , _graylogSocket     :: Socket
      , _graylogHostName   :: Text
      , _graylogChunkSize  :: ChunkSize
      , _graylogCompession :: CompressionType
      }

defaultChunkSize :: ChunkSize
defaultChunkSize = 8192

defaultCompressionType :: CompressionType
defaultCompressionType = GZIP

openGraylog
   :: HostName          -- ^ The host on which graylog is running.
   -> ServiceName       -- ^ The port on which graylog is running.
   -> ChunkSize         -- ^ The maximum size of each UDP datagram.
   -> IO (Either String Graylog)
openGraylog h p cksize
   | cksize < 1024 = return $ Left "ChunkSize must be at least 1024."
   | otherwise     = getAddrInfo Nothing (Just h) (Just p) >>= \case
   []     -> return $ Left "No address info found."
   infos ->
      case find (\i -> addrSocketType i == Datagram) infos of
         Nothing -> return $ Left "No datagram info found for address."
         Just  i -> do
            sock <- socket (addrFamily i) Datagram defaultProtocol
            connect sock (addrAddress i)
            hostname <- getHostName
            return $ Right $ Graylog h p i sock (T.pack hostname) cksize defaultCompressionType

closeGraylog :: Graylog -> IO ()
closeGraylog Graylog{..} = close _graylogSocket
