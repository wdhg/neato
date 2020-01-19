module TestNeato (tests) where

import GA.Neato
import Test.HUnit
import Utils

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
    where
      gene00 = Gene 0 1 0.1 True 0
      gene01 = Gene 0 1 0.6 True 0
      gene10 = Gene 0 2 0.7 True 1
      gene20 = Gene 0 3 0.3 False 2

tests :: Test
tests
  = TestList
    [ "alignGenes" ~: alignGenesTests
    ]
