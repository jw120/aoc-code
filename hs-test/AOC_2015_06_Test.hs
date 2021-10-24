module AOC_2015_06_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2015_06 (apply1, instruction, lights1, start1)

unit_2015_06_lights1_1 :: IO ()
unit_2015_06_lights1_1 = lights1 (apply1 start1 (instruction "turn on 0,0 through 999,999")) @?= 1_000_000

unit_2015_06_lights1_2 :: IO ()
unit_2015_06_lights1_2 = lights1 (apply1 start1 (instruction "toggle 0,0 through 999,0")) @?= 1000
