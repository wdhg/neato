module Utils where

import Test.HUnit

equalCases :: (Show b, Eq b) => (a -> b) -> [(a, b)] -> Test
equalCases func
  = TestList . map (\(input, expected) -> func input ~?= expected)

floatingCases :: (Floating b, Show a, Show b, Ord b)
              => (a -> b) -> [(a, b)] -> Test
floatingCases func
  = TestList . map (uncurry testFunc)
    where
      testFunc input expected
        = abs (output - expected) < epsilon ~? errorMessage
          where
            output = func input
            epsilon = 0.000001
            errorMessage
              = "expected: " ++ show expected ++ "\n" ++
                " but got: " ++ show output
