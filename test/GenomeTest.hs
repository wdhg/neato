module GenomeTest (tests) where

import GA.Neato.Genome
import System.Random
import Test.HUnit
import Utils

gene01a, gene01b, gene02a, gene02b, gene03a, gene03b, gene13a, gene13b :: Gene
gene01a = Gene (0, 1) 0.1 True 0
gene01b = Gene (0, 1) 0.6 True 0
gene02a = Gene (0, 2) 0.7 True 1
gene02b = Gene (0, 2) 0.6 False 1
gene03a = Gene (0, 3) 0.3 False 2
gene03b = Gene (0, 3) 0.8 True 2
gene13a = Gene (1, 3) 0.0 False 3
gene13b = Gene (1, 3) 2.0 False 3

alignGenesTests :: Test
alignGenesTests
  = equalCases (\(genes1, genes2) -> alignGenes (Genome (0,0) genes1) (Genome (0,0) genes2))
    [ ( ( [gene01a, gene03a]
        , [gene01b, gene02a]
        )
      , [ (Just gene01a, Just gene01b)
        , (Nothing, Just gene02a)
        , (Just gene03a, Nothing)
        ])
    , ( ( [gene01a]
        , [gene03a]
        )
      , [ (Just gene01a, Nothing)
        , (Nothing, Just gene03a)
        ]
      )
    ]

calcMeanWeightDeltaTests :: Test
calcMeanWeightDeltaTests
  = floatingCases calcMeanWeightDelta
    [ ( [ (Just gene01a, Just gene01b)
        , (Nothing, Just gene02a)
        , (Just gene03a, Nothing)
        ]
      , 0.5
      )
    , ( [ (Just gene01a, Nothing)
        , (Just gene02a, Just gene02b)
        , (Nothing, Just gene03b)
        , (Just gene13a, Just gene13b)
        ]
      , 1.05
      )
    , ( [ (Just gene01a, Nothing)
        , (Nothing, Just gene02a)
        ]
      , 0.0
      )
    ]

countDisjointExcessTests :: Test
countDisjointExcessTests
  = equalCases countDisjointExcess
    [ ( [ (Just gene01a, Just gene01b)
        , (Nothing, Just gene02a)
        , (Just gene03a, Nothing)
        ]
      , (1, 1)
      )
    , ( [ (Just gene01a, Nothing)
        , (Just gene02a, Just gene02b)
        , (Nothing, Just gene03b)
        , (Just gene13a, Just gene13b)
        ]
      , (2, 0)
      )
    ]

distanceTests :: Test
distanceTests
  = floatingCases (\(g1, g2) -> distance (1, 1, 1) (Genome (0,0) g1) (Genome (0,0) g2))
    [ ( ( [gene01a, gene02a]
        , [gene01a, gene02a]
        )
      , 0.0
      )
    , ( ( [gene01a, gene02a]
        , [gene01b, gene02b]
        )
      , 0.3
      )
    , ( ( [gene01a, gene03a]
        , [gene01b, gene13a]
        )
      , 1.5
      )
    , ( ( [gene01a]
        , [gene02a]
        )
      , 2.0
      )
    ]

-- values are determined in GHCI
-- TODO find a better way of testing eg supplying the random numbers
-- TODO test returned RandomGen is different
perturbWeightTests :: Test
perturbWeightTests
  = floatingCases (fst . uncurry perturbWeight)
    [ ((mkStdGen 0, 0.4), 0.49742936306782304)
    , ((mkStdGen 1, 0.6), 0.5155580750166687)
    , ((mkStdGen 2, -0.7), -0.7652551590782738)
    , ((mkStdGen 3, 1.4), 1.3539316068267833)
    ]

-- values are determined in GHCI
-- TODO find a better way of testing eg supplying the random numbers
-- TODO test returned RandomGen is different
newWeightTests :: Test
newWeightTests
  = floatingCases (fst . newWeight)
    [ (mkStdGen 0, 1.9485872613564603)
    , (mkStdGen 1, -1.6888384996666264)
    , (mkStdGen 2, -1.305103181565478)
    , (mkStdGen 3, -0.9213678634643303)
    ]

-- values are determined in GHCI
-- TODO find a better way of testing eg supplying the random numbers
-- TODO test returned RandomGen is different
mutateWeightTests :: Test
mutateWeightTests
  = floatingCases (fst . uncurry mutateWeight)
    [ ((mkStdGen 0, 0.4), -1.729556574405367)
    , ((mkStdGen 1, 0.6), 0.6799820005890711)
    , ((mkStdGen 2, -0.7), -0.6535582177853032)
    , ((mkStdGen 3, 1.4), 1.412909813122993)
    ]

getNodesTests :: Test
getNodesTests
  = equalCases getNodes
    [ ([gene01a, gene02a, gene03a, gene13a], [0,1,2,3])
    , ([gene01a, gene13a], [0,1,3])
    , ([gene03a], [0,3])
    , ([gene02a, gene13a], [0,1,2,3])
    ]

getNextNodeTests :: Test
getNextNodeTests
  = equalCases (\genes -> getNextNode $ Genome (0,0) genes)
    [ ([gene01a], 2)
    , ([gene13a], 0)
    , ([gene01a, gene02a, gene03a, gene13a], 4)
    ]

getGeneIDTests :: Test
getGeneIDTests
  = equalCases (getGeneID pool)
    [ ((0, 1), (0, pool))
    , ((0, 2), (1, pool))
    , ((0, 3), (2, ((0, 3), 2) : pool))
    , ((1, 2), (2, ((1, 2), 2) : pool))
    ]
      where
        pool = [((0, 1), 0), ((0, 2), 1)]

getUnlinkedTests :: Test
getUnlinkedTests
  = equalCases getUnlinked
    [ (Genome (1, 2) [gene01a, gene02a], [])
    , (Genome (1, 1) [gene01a, gene02a], [(2,1)])
    , (Genome (2, 1) [gene02a, gene13a], [(0,3), (1,2), (3,2)])
    ]

tests :: Test
tests
  = TestList
    [ "alignGenes" ~: alignGenesTests
    , "calcMeanWeightDelta" ~: calcMeanWeightDeltaTests
    , "countDisjointExcess" ~: countDisjointExcessTests
    , "distance" ~: distanceTests
    , "perturbWeight" ~: perturbWeightTests
    , "newWeight" ~: newWeightTests
    , "mutateWeight" ~: mutateWeightTests
    , "getNodes" ~: getNodesTests
    , "getNextNode" ~: getNextNodeTests
    , "getGeneID" ~: getGeneIDTests
    , "getUnlinked" ~: getUnlinkedTests
    ]
