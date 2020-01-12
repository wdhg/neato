import           Test.HUnit
import qualified TestNeato

tests :: Test
tests
  = TestList
  [ TestNeato.tests
  ]

main :: IO Counts
main
  = runTestTT tests
