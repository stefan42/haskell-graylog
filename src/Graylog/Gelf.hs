{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Graylog.Gelf where

import           Data.Aeson        (ToJSON (..), Value (..), genericToJSON,
                                    toJSON)
import           Data.Aeson.Casing
import           Data.Scientific
import           Data.Text         (Text)
import           Data.Time
import           Data.Typeable
import           Data.Vector
import           GHC.Generics

data GELF
   = GELF
      { _gelfVersion          :: Version
      , _gelfHost             :: Text
      , _gelfShortMessage     :: Text
      , _gelfFullMessage      :: Maybe Text
      , _gelfTimestamp        :: Maybe UTCTime
      , _gelfLevel            :: Maybe SyslogLevel
      , _gelfLine             :: Maybe Word
      , _gelfFile             :: Maybe Text
      , _gelfAdditionalFields :: Vector Field
      }
   deriving (Show, Typeable, Generic)

instance ToJSON GELF where
   toJSON = genericToJSON $ aesonPrefix snakeCase

--

data Version
   = Version1x1
   deriving (Eq, Show, Typeable, Generic)

instance ToJSON Version where
   toJSON Version1x1 = String "1.1"

--

data SyslogLevel
   = Emergency
   | Alert
   | Critical
   | Error
   | Warning
   | Notice
   | Informational
   | Debug
   deriving (Eq, Ord, Show, Typeable, Generic)

instance ToJSON SyslogLevel where
   toJSON Emergency     = Number 0
   toJSON Alert         = Number 1
   toJSON Critical      = Number 2
   toJSON Error         = Number 3
   toJSON Warning       = Number 4
   toJSON Notice        = Number 5
   toJSON Informational = Number 6
   toJSON Debug         = Number 7

--

data Field
   = FieldString Text
   | FieldNumber Scientific
   deriving (Eq, Show, Typeable, Generic)

instance ToJSON Field where
   toJSON (FieldString s) = String s
   toJSON (FieldNumber n) = Number n

