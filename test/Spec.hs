import           Test.HUnit
import qualified TestNeato

tests :: Test
tests
  = TestList
  [ "Neato.hs" ~: TestNeato.tests
  ]

main :: IO Counts
main
  = runTestTT tests
