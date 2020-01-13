module TestNeato (tests) where

import AI.Neato
import Data.Bits
import Test.HUnit

getWeightTests :: Test
getWeightTests
  = TestList $ map (\((g, v), e) -> getWeight g v ~?= e)
    [ ((Genome 7 [0.3, 0.5, 0.1] [], 2), 0.1)
    , ((Genome 7 [0.3, 0.5, 0.1] [], 1), 0.5)
    , ((Genome 7 [0.3, 0.5, 0.1] [], 0), 0.3)
    ]

getYoungestGeneTests :: Test
getYoungestGeneTests
  = TestList $ map (\(v, e) -> getYoungestGene v ~?= e)
    [ (bit 4, 4)
    , (bit 3 .|. bit 5, 5)
    , (bit 0, 0)
    , (bit 6 - 1, 5)
    ]

countExcessTests :: Test
countExcessTests
  = TestList $ map (\((g1, g2), e) -> (countExcess (Genome g1 [] []) (Genome g2 [] [])) ~?= e)
    [ ((bit 0, bit 1), 1)
    , ((bit 0, bit 1 .|. bit 2), 2)
    , ((bit 4, bit 4), 0)
    , ((bit 5 .|. bit 9, bit 4 .|. bit 3), 2)
    , ((bit 2 .|. bit 5, bit 1 .|. bit 4), 1)
    ]

countDisjointTests :: Test
countDisjointTests
  = TestList $ map (\((g1, g2), e) -> (countDisjoint (Genome g1 [] []) (Genome g2 [] [])) ~?= e)
    [ ((bit 0, bit 1), 1)
    , ((bit 0, bit 1 .|. bit 2), 1)
    , ((bit 4, bit 4), 0)
    , ((bit 5 .|. bit 9, bit 4 .|. bit 3), 2)
    , ((bit 2 .|. bit 5, bit 1 .|. bit 4), 3)
    ]

tests :: Test
tests
  = TestList
    [ "getYoungestGene" ~: getYoungestGeneTests
    , "countExcess" ~: countExcessTests
    , "countDisjoint" ~: countDisjointTests
    , "getWeight" ~: getWeightTests
    , "getMeanWeights" ~: getMeanWeightDeltaTests
    ]
