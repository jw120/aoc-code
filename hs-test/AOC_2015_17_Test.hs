module AOC_2015_17_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2015_17 (solutions, ways)

unit_2015_17_ways :: IO ()
unit_2015_17_ways = ways 25 [20, 15, 10, 5, 5] @?= 4

unit_2015_17_solutions :: IO ()
unit_2015_17_solutions = solutions 25 [20, 15, 10, 5, 5] @?= [[777]]
