module AOC_2015_08_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2015_08 (deEscape)

unit_2015_07_deEscape_1 :: IO ()
unit_2015_07_deEscape_1 = deEscape "\"\"" @?= ""

unit_2015_07_deEscape_2 :: IO ()
unit_2015_07_deEscape_2 = deEscape "\"abc\"" @?= "abc"

unit_2015_07_deEscape_3 :: IO ()
unit_2015_07_deEscape_3 = deEscape "\"aaa\\\"aaa\"" @?= "aaa\"aaa"

unit_2015_07_deEscape_4 :: IO ()
unit_2015_07_deEscape_4 = deEscape "\"\\x27\"" @?= "'"
