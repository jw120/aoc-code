module AOC_2018_22_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2018_22 (distance, riskLevel)

unit_2018_22_risk1 :: IO ()
unit_2018_22_risk1 = riskLevel 510 (10, 10) @?= 114

unit_2018_22_risk2 :: IO ()
unit_2018_22_risk2 = riskLevel 8103 (9, 758) @?= 7743

unit_2018_22_risk3 :: IO ()
unit_2018_22_risk3 = distance 510 (10, 10) @?= 45
