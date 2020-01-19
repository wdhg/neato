module TestNeato (tests) where

import GA.Neato
import Test.HUnit
import Utils

getGeneTests :: Test
getGeneTests
  = equalCases (\(genes, gene) -> getGene (Genome genes) gene)
    [ (([ Gene 0 1 0.1 True 0
        , Gene 0 2 0.2 True 1
        ], Gene 0 1 0.5 False 0)
      , Just $ Gene 0 1 0.1 True 0)
    , (([], Gene 0 1 0.5 False 0)
      , Nothing)
    ]

tests :: Test
tests
  = TestList
    [ "getGene" ~: getGeneTests
    ]
