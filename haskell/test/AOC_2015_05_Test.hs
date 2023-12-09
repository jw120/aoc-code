module AOC_2015_05_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2015_05 (niceOne, niceTwo)

unit_2015_05_nice1_1 :: IO ()
unit_2015_05_nice1_1 = niceOne "ugknbfddgicrmopn" @?= True

unit_2015_05_nice1_2 :: IO ()
unit_2015_05_nice1_2 = niceOne "aaa" @?= True

unit_2015_05_nice1_3 :: IO ()
unit_2015_05_nice1_3 = niceOne "jchzalrnumimnmhp" @?= False

unit_2015_05_nice1_4 :: IO ()
unit_2015_05_nice1_4 = niceOne "haegwjzuvuyypxyu" @?= False

unit_2015_05_nice1_5 :: IO ()
unit_2015_05_nice1_5 = niceOne "dvszwmarrgswjxmb" @?= False

unit_2015_05_nice2_1 :: IO ()
unit_2015_05_nice2_1 = niceTwo "qjhvhtzxzqqjkmpb" @?= True

unit_2015_05_nice2_2 :: IO ()
unit_2015_05_nice2_2 = niceTwo "xxyxx" @?= True

unit_2015_05_nice2_3 :: IO ()
unit_2015_05_nice2_3 = niceTwo "uurcxstgmygtbstg" @?= False

unit_2015_05_nice2_4 :: IO ()
unit_2015_05_nice2_4 = niceTwo "ieodomkazucvgmuy" @?= False
