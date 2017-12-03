import qualified Moba.Client.Tests
import           Test.Framework    (defaultMain)

main :: IO ()
main = defaultMain
  [ Moba.Client.Tests.tests
  ]
