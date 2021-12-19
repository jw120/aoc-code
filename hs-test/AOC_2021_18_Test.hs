module AOC_2021_18_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2021_18 (magnitude, readSnail)

unit_2021_18_magnitude1 :: IO ()
unit_2021_18_magnitude1 = magnitude (readSnail "[9,1]") @?= 29

unit_2021_18_magnitude2 :: IO ()
unit_2021_18_magnitude2 = magnitude (readSnail "[1,9]") @?= 21

unit_2021_18_magnitude3 :: IO ()
unit_2021_18_magnitude3 = magnitude (readSnail "[[9,1],[1,9]]") @?= 129

unit_2021_18_magnitude4 :: IO ()
unit_2021_18_magnitude4 = magnitude (readSnail "[[1,2],[[3,4],5]]") @?= 143

unit_2021_18_magnitude5 :: IO ()
unit_2021_18_magnitude5 = magnitude (readSnail "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]") @?= 1384

unit_2021_18_magnitude6 :: IO ()
unit_2021_18_magnitude6 = magnitude (readSnail "[[[[1,1],[2,2]],[3,3]],[4,4]]") @?= 445

unit_2021_18_magnitude7 :: IO ()
unit_2021_18_magnitude7 = magnitude (readSnail "[[[[3,0],[5,3]],[4,4]],[5,5]]") @?= 791

unit_2021_18_magnitude8 :: IO ()
unit_2021_18_magnitude8 = magnitude (readSnail "[[[[5,0],[7,4]],[5,5]],[6,6]]") @?= 1137

unit_2021_18_magnitude9 :: IO ()
unit_2021_18_magnitude9 = magnitude (readSnail "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]") @?= 3488
