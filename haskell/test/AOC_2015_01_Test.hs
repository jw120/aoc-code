module AOC_2015_01_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2015_01 (firstVisit)

unit_2015_01_firstVisit_simple :: IO ()
unit_2015_01_firstVisit_simple = firstVisit ")" @?= 1

unit_2015_01_firstVisit_standard :: IO ()
unit_2015_01_firstVisit_standard = firstVisit "()())" @?= 5
