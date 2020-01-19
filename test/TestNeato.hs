module TestNeato (tests) where

import GA.Neato
import Test.HUnit
import Utils

-- test genes
gene00 = Gene 0 1 0.1 True 0
gene01 = Gene 0 1 0.6 True 0
gene10 = Gene 0 2 0.7 True 1
gene20 = Gene 0 3 0.3 False 2

tests :: Test
tests
  = TestList
    [ "alignGenes" ~: alignGenesTests
    , "splitExcess" ~: splitExcessTests
    ]
