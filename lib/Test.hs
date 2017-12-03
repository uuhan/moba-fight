import qualified Moba.InternalTest
import           Test.Framework    (defaultMain)

main :: IO ()
main = defaultMain
  [ Moba.InternalTest.tests
  ]
