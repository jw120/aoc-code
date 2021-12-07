module AOC_2021_06_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2021_06 (alive, alive', step)

test1 :: [Int]
test1 = [3, 4, 3, 1, 2]

unit_2021_06_step1 :: IO ()
unit_2021_06_step1 = step test1 @?= [2, 3, 2, 0, 1]

unit_2021_06_step2 :: IO ()
unit_2021_06_step2 =
    iterate step test1 !! 2
        @?= [1, 2, 1, 6, 0, 8]

unit_2021_06_step3 :: IO ()
unit_2021_06_step3 =
    iterate step test1 !! 18
        @?= [6, 0, 6, 4, 5, 6, 0, 1, 1, 2, 6, 0, 1, 1, 1, 2, 2, 3, 3, 4, 6, 7, 8, 8, 8, 8]

-- unit_2021_06_step4 :: IO ()
-- unit_2021_06_step4 = length (iterate step test1 !! 256) @?= 26984457539

unit_2021_06_alive1 :: IO ()
unit_2021_06_alive1 = alive 1 test1 @?= 5

unit_2021_06_alive2 :: IO ()
unit_2021_06_alive2 = alive 2 test1 @?= 6

unit_2021_06_alive3 :: IO ()
unit_2021_06_alive3 = alive 18 test1 @?= 26

-- unit_2021_06_alive4 :: IO ()
-- unit_2021_06_alive4 = alive 256 test1 @?= 26984457539

unit_2021_06_alive'1 :: IO ()
unit_2021_06_alive'1 = alive' 1 test1 @?= 5

unit_2021_06_alive'2 :: IO ()
unit_2021_06_alive'2 = alive' 2 test1 @?= 6

unit_2021_06_alive'3 :: IO ()
unit_2021_06_alive'3 = alive' 18 test1 @?= 26

unit_2021_06_alive'4 :: IO ()
unit_2021_06_alive'4 = alive' 256 test1 @?= 26984457539