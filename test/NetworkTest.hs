module NetworkTest (tests) where

import qualified Data.Map         as Map
import           GA.Neato.Network
import           Test.HUnit
import           Utils

testNetwork0 :: Network
testNetwork0
  = Network (2, 1) $ Map.fromList
    [ (2, Map.fromList [(0,4.0), (3,3.0)])
    , (3, Map.fromList [(1, 0.6)])
    ]

setNodeTests :: Test
setNodeTests
  = equalCases ((flip $ uncurry setNode) nodes)
    [ ((0,1.0), [1.0,1.0,2.0,3.0])
    , ((1,1.0), [0.0,1.0,2.0,3.0])
    , ((2,1.0), [0.0,1.0,1.0,3.0])
    , ((3,1.0), [0.0,1.0,2.0,1.0])
    ]
      where
        nodes
          = [0.0..3.0]

computeNodeTests :: Test
computeNodeTests
  = floatingCases (\n -> (!! n) $ snd $ computeNode id (testNetwork0, nodes) n)
    [ (0, 3.2)
    , (1, 5.7)
    , (3, 0.6 * 5.7)
    , (2, (3.0 * 0.6 * 5.7) + (4.0 * 3.2))
    ]
      where
        nodes
          = [3.2, 5.7, 0.0, 0.0]

runTests :: Test
runTests
  = floatingListCases (run id testNetwork0 inputs)
    [

    ]
      where
        inputs
          = []

tests :: Test
tests
  = TestList
    [ "setNode" ~: setNodeTests
    , "computeNode" ~: computeNodeTests
    ]
