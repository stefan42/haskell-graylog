{-# LANGUAGE RecordWildCards #-}

module Graylog.Types
   ( Graylog
   , _graylogHost
   , _graylogPort
   , _graylogAddress

   , openGraylog
   , closeGraylog
   ) where

import           Data.Text      (Text)
import qualified Data.Text      as T
import           Network.BSD
import           Network.Socket

data Graylog
   = Graylog
      { _graylogHost     :: String
      , _graylogPort     :: String
      , _graylogAddress  :: AddrInfo
      , _graylogSocket   :: Socket
      , _graylogHostName :: Text
      }

openGraylog :: HostName -> ServiceName -> IO (Either String Graylog)
openGraylog host port = do
   infos <- getAddrInfo Nothing (Just host) (Just port)
   case infos of
      []     -> return $ Left "No address info found."
      [info] -> do
         sock <- socket (addrFamily info) Datagram defaultProtocol
         connect sock (addrAddress info)
         hostname <- getHostName
         return $ Right $ Graylog host port info sock (T.pack hostname)
      _      -> return $ Left "Too many address infos found."

closeGraylog :: Graylog -> IO ()
closeGraylog Graylog{..} = close _graylogSocket


