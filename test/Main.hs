import qualified Test.Graylog.UDP (tests)

import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Graylog Library"
   [ Test.Graylog.UDP.tests
   ]

