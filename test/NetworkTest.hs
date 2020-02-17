module NetworkTest (tests) where

import GA.Neato.Network
import Test.HUnit
import Utils

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
  = floatingCases (\n -> (!! n) $ snd $ computeNode id (network, nodes) n)
    [ (0, 3.2)
    , (1, 5.7)
    , (3, 0.6 * 5.7)
    , (2, (3.0 * 0.6 * 5.7) + (4.0 * 3.2))
    ]
      where
        network
          = Network (2, 1)
            [ (2, [(0,4.0), (3,3.0)])
            , (3, [(1, 0.6)])
            ]
        nodes
          = [3.2, 5.7, 0.0, 0.0]

tests :: Test
tests
  = TestList
    [ "setNode" ~: setNodeTests
    , "computeNode" ~: computeNodeTests
    ]
