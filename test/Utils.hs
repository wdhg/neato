module Utils (equalCases, floatingCases) where

import Test.HUnit

floatingCase :: (Floating a, Show a, Ord a) => a -> a -> Test
floatingCase value expected
  = abs (value - expected) < epsilon ~? errorMessage
    where
      epsilon = 0.000001
      errorMessage
        = "expected: " ++ show expected ++ "\n" ++
          " but got: " ++ show value

equalCases :: (Show b, Eq b) => (a -> b) -> [(a, b)] -> Test
equalCases func
  = TestList . map (\(input, expected) -> func input ~?= expected)

floatingCases :: (Floating a, Show a, Ord a)
              => (b -> a) -> [(b, a)] -> Test
floatingCases func
  = TestList . map testFunc
    where
      testFunc (input, expected)
        = floatingCase (func input) expected

floatingListCases :: (Floating a, Show a, Ord a)
              => (b -> [a]) -> [(b, [a])] -> Test
floatingListCases func
  = TestList . map testFunc
    where
      testFunc (input, expected)
        = TestList $ zipWith floatingCase (func input) expected
