module NetworkTest (tests) where

import qualified Data.Map         as Map
import           GA.Neato.Genome
import           GA.Neato.Network
import           Test.HUnit
import           Utils

{-
  [0]       [1]
     \       |
      \     [3]
       \   /
        [2]
-}

testGenome0 :: Genome
testGenome0
  = Genome (2, 1)
    [ Gene (0, 2) 4.0 True 0
    , Gene (1, 2) 3.6 False 1
    , Gene (1, 3) 0.6 True 2
    , Gene (3, 2) 3.0 True 3
    ]

testNetwork0 :: Network
testNetwork0
  = Network (2, 1) $ Map.fromList
    [ (2, Map.fromList [(0,4.0), (3,3.0)])
    , (3, Map.fromList [(1, 0.6)])
    ]

computeNodeTests :: Test
computeNodeTests
  = floatingCases (\n -> (map snd $ Map.toList $ computeNode id testNetwork0 nodes n) !! n)
    [ (0, 3.2)
    , (1, 5.7)
    , (3, 0.6 * 5.7)
    , (2, (3.0 * 0.6 * 5.7) + (4.0 * 3.2))
    ]
      where
        nodes
          = Map.fromList $ zip [0..] [3.2, 5.7, 0.0, 0.0]

runTests :: Test
runTests
  = floatingListCases (run id testNetwork0)
    [ ([0.0, 0.0], [0.0])
    , ([0.5, 0.4], [4.0 * 0.5 + 3.0 * (0.6 * 0.4)])
    ]

buildNetworkTests :: Test
buildNetworkTests
  = equalCases buildNetwork
    [ (testGenome0, testNetwork0)
    ]

tests :: Test
tests
  = TestList
    [ "computeNode" ~: computeNodeTests
    , "run" ~: runTests
    , "buildNetwork" ~: buildNetworkTests
    ]
