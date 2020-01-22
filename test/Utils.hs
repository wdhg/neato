module Utils where

import System.Random
import Test.HUnit

data DummyGen
  = DummyGen Int Int [Int]
    deriving Show

instance RandomGen DummyGen where
  next (DummyGen lower upper [])
    = (0, DummyGen lower upper [])
  next (DummyGen lower upper (value : values))
    = (value, DummyGen lower upper values)
  genRange (DummyGen lower upper _)
    = (lower, upper)
  split dummy@(DummyGen lower upper values)
    = (dummy, DummyGen lower upper $ tail values)

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
