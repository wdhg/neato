module Utils (equalCases, floatingCases) where

import Test.HUnit

floatingEqual :: (Floating a, Ord a) => a -> a -> Bool
floatingEqual value expected
  = abs (value - expected) < epsilon
    where
      epsilon = 0.000001

equalCases :: (Show b, Eq b) => (a -> b) -> [(a, b)] -> Test
equalCases func
  = TestList . map (\(input, expected) -> func input ~?= expected)

floatingCases :: (Floating b, Show a, Show b, Ord b)
              => (a -> b) -> [(a, b)] -> Test
floatingCases func
  = TestList . map testFunc
    where
      testFunc (input, expected)
        = floatingEqual output expected ~? errorMessage
          where
            output
              = func input
            errorMessage
              = "expected: " ++ show expected ++ "\n" ++
                " but got: " ++ show output
