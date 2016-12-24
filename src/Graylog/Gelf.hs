{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Default formatting for Graylog messages,
-- see http://docs.graylog.org/en/latest/pages/gelf.html
module Graylog.Gelf where

import           Data.Aeson        (ToJSON (..), Value (..), object, (.=), toJSON)
import           Data.Text         (Text, cons)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Typeable
import           GHC.Generics

data GELF
   = GELF
      { _gelfVersion      :: Version
      , _gelfHost         :: Text
      , _gelfShortMessage :: Text
      , _gelfFullMessage  :: Maybe Text
      , _gelfTimestamp    :: Maybe UTCTime
      , _gelfLevel        :: Maybe SyslogLevel
      , _gelfLine         :: Maybe Word
      , _gelfFile         :: Maybe Text
      , _gelfAdditionals  :: [Field]
      }
   deriving (Show, Typeable, Generic)

instance ToJSON GELF where
   toJSON gelf = object
      $ ("version"       .= (_gelfVersion gelf))
      : ("host"          .= (_gelfHost gelf))
      : ("short_message" .= (_gelfShortMessage gelf))
      : ("full_message"  .= (_gelfFullMessage gelf))
      : ("timestamp"     .= (convertedTimestamp $ _gelfTimestamp gelf))
      : ("level"         .= (_gelfLevel gelf))
      : ("line"          .= (_gelfLine gelf))
      : ("file"          .= (_gelfFile gelf))
      : map (\(n,v) -> ('_' `cons` n, toJSON v)) (_gelfAdditionals gelf)
     where
      convertedTimestamp :: Maybe UTCTime -> Maybe Double
      convertedTimestamp Nothing   = Nothing
      convertedTimestamp (Just ts) = Just
          $ (/1000)
          $ fromIntegral
          $ (floor::(RealFrac a)=>a->Integer)
          $ (*1000)
          $ (realToFrac::(Real a) => a->Double)
          $ utcTimeToPOSIXSeconds ts

--

data FieldValue
  = StringField Text
  | NumberField Word
  deriving (Show, Typeable, Generic)

instance ToJSON FieldValue where
   toJSON (StringField s) = toJSON s
   toJSON (NumberField n) = toJSON n

type Field = (Text, FieldValue)

gelfSField :: Text -> Text -> Field
gelfSField n v = (n, StringField v)

gelfNField :: Text -> Word -> Field
gelfNField n v = (n, NumberField v)

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

simpleGelf
   :: Text     -- ^ Hostname
   -> Text     -- ^ Short message
   -> GELF
simpleGelf host short =
   GELF Version1x1 host short Nothing Nothing Nothing Nothing Nothing []
