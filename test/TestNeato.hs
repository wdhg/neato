module TestNeato (tests) where

import Data.Bits
import GA.Neato
import Test.HUnit
import Utils

getIndicesTests :: Test
getIndicesTests
  = equalCases getIndices
    [ (bit 0 .|. bit 1 .|. bit 2, [0,1,2])
    , (bit 0, [0])
    , (0, [])
    , (bit 3 .|. bit 6, [3, 6])
    ]

getWeightTests :: Test
getWeightTests
  = equalCases (uncurry getWeight)
    [ ((Genome 7 [0.3, 0.5, 0.1] [], 2), 0.1)
    , ((Genome 7 [0.3, 0.5, 0.1] [], 1), 0.5)
    , ((Genome 7 [0.3, 0.5, 0.1] [], 0), 0.3)
    ]

getYoungestGeneTests :: Test
getYoungestGeneTests
  = equalCases getYoungestGene
    [ (bit 4, 4)
    , (bit 3 .|. bit 5, 5)
    , (bit 0, 0)
    , (bit 6 - 1, 5)
    ]

countExcessTests :: Test
countExcessTests
  = equalCases (\(g1, g2) -> countExcess (Genome g1 [] []) (Genome g2 [] []))
    [ ((bit 0, bit 1), 1)
    , ((bit 0, bit 1 .|. bit 2), 2)
    , ((bit 4, bit 4), 0)
    , ((bit 5 .|. bit 9, bit 4 .|. bit 3), 2)
    , ((bit 2 .|. bit 5, bit 1 .|. bit 4), 1)
    ]

countDisjointTests :: Test
countDisjointTests
  = equalCases (\(g1, g2) -> countDisjoint (Genome g1 [] []) (Genome g2 [] []))
    [ ((bit 0, bit 1), 1)
    , ((bit 0, bit 1 .|. bit 2), 1)
    , ((bit 4, bit 4), 0)
    , ((bit 5 .|. bit 9, bit 4 .|. bit 3), 2)
    , ((bit 2 .|. bit 5, bit 1 .|. bit 4), 3)
    ]

getMeanWeightDeltaTests :: Test
getMeanWeightDeltaTests
  = floatingCases (uncurry calcMeanWeightDelta)
    [ ((Genome 1 [0.4] [], Genome 1 [0.6] []), 0.2)
    , ((Genome 3 [0.4, 0.5] [], Genome 1 [0.6] []), 0.2)
    , ((Genome 3 [0.4, 0.5] [], Genome 3 [0.6, 0.9] []), 0.3)
    ]

getGenesTests :: Test
getGenesTests
  = equalCases (\genes -> getGenes pool $ Genome genes [] [])
    [ (bit 0 .|. bit 3 .|. bit 5, [(0, 0), (1, 2), (2, 3)])
    , (0, [])
    , (bit 2 .|. bit 4, [(0, 2), (1, 3)])
    ]
    where
      pool = [(0, 0), (0, 1), (0, 2), (1, 2), (1, 3), (2, 3)]

tests :: Test
tests
  = TestList
    [ "getIndices" ~: getIndicesTests
    , "getYoungestGene" ~: getYoungestGeneTests
    , "countExcess" ~: countExcessTests
    , "countDisjoint" ~: countDisjointTests
    , "getWeight" ~: getWeightTests
    , "getMeanWeights" ~: getMeanWeightDeltaTests
    , "getGenes" ~: getGenesTests
    ]
