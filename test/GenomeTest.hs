module GenomeTest (tests) where

import GA.Neato.Genome
import Test.HUnit
import Utils

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
    ]

tests :: Test
tests
  = TestList
    [ "alignGenes" ~: alignGenesTests
    , "calcMeanWeightDelta" ~: calcMeanWeightDeltaTests
    ]
