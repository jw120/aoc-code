module AOC_2018_22_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2018_22 (riskLevel)

unit_2015_22_risk :: IO ()
unit_2015_22_risk = riskLevel 510 (10, 10) @?= 114
