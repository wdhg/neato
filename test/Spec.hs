import qualified GenomeTest
import qualified NetworkTest
import           Test.HUnit

tests :: Test
tests
  = TestList
  [ "Neato/Genome.hs" ~: GenomeTest.tests
  , "Neato/Network.hs" ~: NetworkTest.tests
  ]

main :: IO Counts
main
  = runTestTT tests
