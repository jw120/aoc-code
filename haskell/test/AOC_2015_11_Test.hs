module AOC_2015_11_Test where

import AOC_2015_11 (increment, nextValid, valid)
import Test.Tasty.HUnit ((@?=))

unit_2015_11_increment :: IO ()
unit_2015_11_increment = map increment ["xx", "xy", "xz", "ya", "ayb"] @?= ["xy", "xz", "ya", "yb", "ayc"]

unit_2015_11_valid :: IO ()
unit_2015_11_valid = map valid ["hijklmmn", "abbceffg", "abbcegjk"] @?= [False, False, False]

unit_2015_11_nextValid :: IO ()
unit_2015_11_nextValid = map nextValid ["abcdefgh", "ghijklmn"] @?= ["abcdffaa", "ghjaabcc"]
