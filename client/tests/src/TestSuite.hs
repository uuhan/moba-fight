import           Test.Framework            (defaultMain)
import qualified Client.Moba.Internal.Tests
import qualified Client.Moba.Tests

main :: IO ()
main = defaultMain
  [ Client.Moba.Tests.tests
  , Client.Moba.Internal.Tests.tests
  ]
