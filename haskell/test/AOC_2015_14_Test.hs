module AOC_2015_14_Test where

import Test.Tasty.HUnit ((@?=))

import Data.Text (Text)

import AOC_2015_14 (Reindeer (..), advance, pReindeer)

import Utilities (parseOrStop)

test :: [Reindeer]
test =
    map
        (parseOrStop pReindeer)
        [ "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
        , "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
        ]

distances :: [Reindeer] -> [(Text, Int)]
distances = map (\d -> (name d, distance d))

unit_2015_14_deer_0 :: IO ()
unit_2015_14_deer_0 = distances (advance 0 test) @?= [("Comet", 0), ("Dancer", 0)]

unit_2015_14_deer_1 :: IO ()
unit_2015_14_deer_1 = distances (advance 1 test) @?= [("Comet", 14), ("Dancer", 16)]

unit_2015_14_deer_10 :: IO ()
unit_2015_14_deer_10 = distances (advance 10 test) @?= [("Comet", 140), ("Dancer", 160)]

unit_2015_14_deer_11 :: IO ()
unit_2015_14_deer_11 = distances (advance 11 test) @?= [("Comet", 140), ("Dancer", 176)]

unit_2015_14_deer_12 :: IO ()
unit_2015_14_deer_12 = distances (advance 12 test) @?= [("Comet", 140), ("Dancer", 176)]

unit_2015_14_deer_137 :: IO ()
unit_2015_14_deer_137 = distances (advance 137 test) @?= [("Comet", 140), ("Dancer", 176)]

unit_2015_14_deer_138 :: IO ()
unit_2015_14_deer_138 = distances (advance 138 test) @?= [("Comet", 154), ("Dancer", 176)]

unit_2015_14_deer_148 :: IO ()
unit_2015_14_deer_148 = distances (advance 148 test) @?= [("Comet", 280), ("Dancer", 176)]

unit_2015_14_deer_174 :: IO ()
unit_2015_14_deer_174 = distances (advance 174 test) @?= [("Comet", 280), ("Dancer", 192)]

unit_2015_14_deer_184 :: IO ()
unit_2015_14_deer_184 = distances (advance 184 test) @?= [("Comet", 280), ("Dancer", 352)]

unit_2015_14_deer_1000 :: IO ()
unit_2015_14_deer_1000 = distances (advance 1000 test) @?= [("Comet", 1120), ("Dancer", 1056)]
