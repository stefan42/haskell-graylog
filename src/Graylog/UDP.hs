module Graylog.UDP
   ( Graylog (..)
   , sendLog

   , module Export
   ) where

import           Graylog.Gelf  as Export
import           Graylog.Types as Export

sendLog :: Graylog -> GELF -> IO ()
sendLog glog msg = undefined

