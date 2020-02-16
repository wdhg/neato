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

tests :: Test
tests
  = TestList
    [ "setNode" ~: setNodeTests
    ]
