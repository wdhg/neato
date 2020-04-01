module NetworkTest (tests) where

import qualified Data.Map         as Map
import           GA.Neato.Genome
import           GA.Neato.Network
import           Test.HUnit
import           Utils

testGenome0 :: Genome
testGenome0
  = Genome (2, 1)
    [ Gene (0, 2) 4.0 True 0
    , Gene (1, 2) 3.6 False 1
    , Gene (1, 3) 0.6 True 2
    , Gene (3, 2) 3.0 True 3
    ]

testGenome1 :: Genome
testGenome1
  = Genome (2, 2)
    [ Gene (0, 2) 0.6 False 0
    , Gene (1, 2) 0.3 False 1
    , Gene (0, 3) 0.1 True 2
    , Gene (1, 3) 2.3 True 3
    , Gene (0, 4) 4.2 True 4
    , Gene (1, 4) 0.9 True 5
    , Gene (4, 2) 5.3 True 6
    ]

testNetwork0 :: Network
testNetwork0
  = Network (2, 1) $ Map.fromList
    [ (2, Map.fromList [(0,4.0), (3,3.0)])
    , (3, Map.fromList [(1, 0.6)])
    ]

testNetwork1 :: Network
testNetwork1
  = Network (2, 2) $ Map.fromList
    [ (2, Map.fromList [(4,5.3)])
    , (3, Map.fromList [(0,0.1), (1,2.3)])
    , (4, Map.fromList [(0,4.2), (1,0.9)])
    ]

computeNodeTests :: Test
computeNodeTests
  = floatingCases (\(network, node) -> (getValues $ computeNode id network nodes node) !! node)
    [ ((testNetwork0, 0), 3.2)
    , ((testNetwork0, 1), 5.7)
    , ((testNetwork0, 3), 0.6 * 5.7)
    , ((testNetwork0, 2), (3.0 * 0.6 * 5.7) + (4.0 * 3.2))
    ]
      where
        getValues
          = map snd . Map.toList
        nodes
          = Map.fromList $ zip [0..] [3.2, 5.7, 0.0, 0.0]

runTests :: Test
runTests
  = floatingListCases (uncurry $ run id)
    [ ((testNetwork0, [0.0, 0.0]), [0.0])
    , ((testNetwork0, [0.5, 0.4]), [4.0 * 0.5 + 3.0 * (0.6 * 0.4)])
    ]

buildNetworkTests :: Test
buildNetworkTests
  = equalCases buildNetwork
    [ (testGenome0, testNetwork0)
    , (testGenome1, testNetwork1)
    ]

tests :: Test
tests
  = TestList
    [ "computeNode" ~: computeNodeTests
    , "run" ~: runTests
    , "buildNetwork" ~: buildNetworkTests
    ]
