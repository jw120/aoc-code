module AOC_2015_20_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2015_20 (presents)

unit_2015_20_presents :: IO ()
unit_2015_20_presents = map presents [1 .. 9] @?= [10, 30, 40, 70, 60, 120, 80, 150, 130]
