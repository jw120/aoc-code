module AOC_2018_22_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2018_22 (riskLevel)

unit_2015_22_risk0 :: IO ()
unit_2015_22_risk0 = riskLevel 510 (3, 3) @?= 0

unit_2015_22_risk1 :: IO ()
unit_2015_22_risk1 = riskLevel 510 (10, 10) @?= 114

unit_2015_22_risk2 :: IO ()
unit_2015_22_risk2 = riskLevel 8103 (9, 758) @?= 7743
