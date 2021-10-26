module AOC_2015_10_Test where

import AOC_2015_10 (lookSay)
import Test.Tasty.HUnit ((@?=))

unit_2015_10_lookSay_1 :: IO ()
unit_2015_10_lookSay_1 = lookSay [1] @?= ([1, 1] :: [Int])

unit_2015_10_lookSay_2 :: IO ()
unit_2015_10_lookSay_2 = lookSay [1, 1] @?= ([2, 1] :: [Int])

unit_2015_10_lookSay_3 :: IO ()
unit_2015_10_lookSay_3 = lookSay [2, 1] @?= ([1, 2, 1, 1] :: [Int])

unit_2015_10_lookSay_4 :: IO ()
unit_2015_10_lookSay_4 = lookSay [1, 2, 1, 1] @?= ([1, 1, 1, 2, 2, 1] :: [Int])

unit_2015_10_lookSay_5 :: IO ()
unit_2015_10_lookSay_5 = lookSay [1, 1, 1, 2, 2, 1] @?= ([3, 1, 2, 2, 1, 1] :: [Int])
