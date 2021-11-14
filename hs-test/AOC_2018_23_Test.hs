module AOC_2018_23_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2018_23 (Bot, inMaxRange, pBot)
import Utilities (parseOrStop)

test1 :: [Bot]
test1 =
    map
        (parseOrStop pBot)
        [ "pos=<10,12,12>, r=2"
        , "pos=<12,14,12>, r=2"
        , "pos=<16,12,12>, r=4"
        , "pos=<14,14,14>, r=6"
        , "pos=<50,50,50>, r=200"
        , "pos=<10,10,10>, r=5"
        ]

unit_2018_23_maxRange1 :: IO ()
unit_2018_23_maxRange1 = inMaxRange test1 @?= (0, [])
