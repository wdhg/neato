import qualified GenomeTest
import           Test.HUnit

tests :: Test
tests
  = TestList
  [ "Neato/Genome.hs" ~: GenomeTest.tests
  ]

main :: IO Counts
main
  = runTestTT tests
