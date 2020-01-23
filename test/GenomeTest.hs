module GenomeTest (tests) where

import GA.Neato.Genome
import System.Random
import Test.HUnit
import Utils

gene00, gene01, gene10, gene11, gene20, gene21, gene30, gene31 :: Gene
gene00 = Gene 0 1 0.1 True 0
gene01 = Gene 0 1 0.6 True 0
gene10 = Gene 0 2 0.7 True 1
gene11 = Gene 0 2 0.6 False 1
gene20 = Gene 0 3 0.3 False 2
gene21 = Gene 0 3 0.8 True 2
gene30 = Gene 1 3 0.0 False 3
gene31 = Gene 1 3 2.0 False 3

alignGenesTests :: Test
alignGenesTests
  = equalCases (\(genes1, genes2) -> alignGenes (Genome genes1) (Genome genes2))
    [ ( ( [gene00, gene20]
        , [gene01, gene10]
        )
      , [ (Just gene00, Just gene01)
        , (Nothing, Just gene10)
        , (Just gene20, Nothing)
        ])
    , ( ( [gene00]
        , [gene20]
        )
      , [ (Just gene00, Nothing)
        , (Nothing, Just gene20)
        ]
      )
    ]

calcMeanWeightDeltaTests :: Test
calcMeanWeightDeltaTests
  = floatingCases calcMeanWeightDelta
    [ ( [ (Just gene00, Just gene01)
        , (Nothing, Just gene10)
        , (Just gene20, Nothing)
        ]
      , 0.5
      )
    , ( [ (Just gene00, Nothing)
        , (Just gene10, Just gene11)
        , (Nothing, Just gene21)
        , (Just gene30, Just gene31)
        ]
      , 1.05
      )
    , ( [ (Just gene00, Nothing)
        , (Nothing, Just gene10)
        ]
      , 0.0
      )
    ]

countDisjointExcessTests :: Test
countDisjointExcessTests
  = equalCases countDisjointExcess
    [ ( [ (Just gene00, Just gene01)
        , (Nothing, Just gene10)
        , (Just gene20, Nothing)
        ]
      , (1, 1)
      )
    , ( [ (Just gene00, Nothing)
        , (Just gene10, Just gene11)
        , (Nothing, Just gene21)
        , (Just gene30, Just gene31)
        ]
      , (2, 0)
      )
    ]

distanceTests :: Test
distanceTests
  = floatingCases (\(g1, g2) -> distance (1, 1, 1) (Genome g1) (Genome g2))
    [ ( ( [gene00, gene10]
        , [gene00, gene10]
        )
      , 0.0
      )
    , ( ( [gene00, gene10]
        , [gene01, gene11]
        )
      , 0.3
      )
    , ( ( [gene00, gene20]
        , [gene01, gene30]
        )
      , 1.5
      )
    , ( ( [gene00]
        , [gene10]
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
    ]
