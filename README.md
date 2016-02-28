# Graylog
This library provides support for sending GELF formatted messages to Graylog.
Currently the UDP chunked method is the only method supported.

```haskell
import Graylog.UDP

main :: IO ()
main = do
   eglog <- openGraylog "192.168.99.100" "12201" defaultChunkSize
   case eglog of
      Left  e -> assertFailure e
      Right g -> sendLog g sample >> closeGraylog g
   where
      sample = simpleGelf "localhost" "hello world!"
```
