module TestNeato (tests) where

import AI.Neato
import Data.Bits
import Test.HUnit

getYoungestGeneTests :: Test
getYoungestGeneTests
  = TestList $ map (\(v, e) -> (getYoungestGene $ Genome v [] []) ~?= e)
    [ (bit 4, 4)
    , (bit 3 .|. bit 5, 5)
    , (bit 0, 0)
    , (bit 6 - 1, 5)
    ]

tests :: Test
tests
  = TestList
    [ getYoungestGeneTests
    ]
