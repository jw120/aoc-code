module AOC_2015_09_Test where

import Test.Tasty.HUnit ((@?=))

import Data.Text (Text)

import AOC_2015_09 (Extreme (..), makeDistance, shortestPath)

distances :: [((Text, Text), Int)]
distances =
    [ (("London", "Dublin"), 464)
    , (("London", "Belfast"), 518)
    , (("Dublin", "Belfast"), 141)
    ]

cities :: [Text]
cities = ["London", "Dublin", "Belfast"]

unit_2015_09_distance_1 :: IO ()
unit_2015_09_distance_1 = shortestPath Min (makeDistance distances) cities @?= 605
