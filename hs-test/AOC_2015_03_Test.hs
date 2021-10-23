module AOC_2015_03_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2015_03 (houses, robotHouses, route)

unit_2015_03_houses_1 :: IO ()
unit_2015_03_houses_1 = (houses . route) ">" @?= 2

unit_2015_03_houses_2 :: IO ()
unit_2015_03_houses_2 = (houses . route) "^>v<" @?= 4

unit_2015_03_houses_3 :: IO ()
unit_2015_03_houses_3 = (houses . route) "^v^v^v^v^v" @?= 2

unit_2015_03_robotHouses_1 :: IO ()
unit_2015_03_robotHouses_1 = (robotHouses . route) "^v" @?= 3

unit_2015_03_robotHouses_2 :: IO ()
unit_2015_03_robotHouses_2 = (robotHouses . route) "^>v<" @?= 3

unit_2015_03_robotHouses_3 :: IO ()
unit_2015_03_robotHouses_3 = (robotHouses . route) "^v^v^v^v^v" @?= 11
