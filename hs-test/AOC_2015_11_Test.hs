module AOC_2015_11_Test where

import AOC_2015_11 (increment)
import Test.Tasty.HUnit ((@?=))

unit_2015_11_increment :: IO ()
unit_2015_11_increment = map increment ["xx", "xy", "xz", "ya", "ayb"] @?= ["xy", "xz", "ya", "yb", "ayc"]
