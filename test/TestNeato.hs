module TestNeato (tests) where

import AI.Neato
import Data.Bits
import Test.HUnit

getYoungestGeneTests :: Test
getYoungestGeneTests
  = TestList $ map (\(v, e) -> getYoungestGene v ~?= e)
    [ (bit 4, 4)
    , (bit 3 .|. bit 5, 5)
    , (bit 0, 0)
    , (bit 6 - 1, 5)
    ]

countExcessTest :: Test
countExcessTest
  = TestList $ map (\((g1, g2), e) -> (countExcess (Genome g1 [] []) (Genome g2 [] [])) ~?= e)
    [ ((bit 0, bit 1), 1)
    , ((bit 0, bit 1 .|. bit 2), 2)
    , ((bit 4, bit 4), 0)
    , ((bit 5 .|. bit 9, bit 4 .|. bit 3), 2)
    , ((bit 2 .|. bit 5, bit 1 .|. bit 4), 1)
    ]

countDisjointTest :: Test
countDisjointTest
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
    [ getYoungestGeneTests
    , countExcessTest
    , countDisjointTest
    ]
