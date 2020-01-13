module Utils where

import Test.HUnit

equalCases :: (Show b, Eq b) => (a -> b) -> [(a, b)] -> Test
equalCases func
  = TestList . map (\(input, expected) -> func input ~?= expected)
