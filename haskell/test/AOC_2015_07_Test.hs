module AOC_2015_07_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2015_07 (booklet, value)

import Data.Text (Text)
import Data.Text qualified as T (pack)

exampleInput :: [Text]
exampleInput =
    [ "123 -> x"
    , "456 -> y"
    , "x AND y -> d"
    , "x OR y -> e"
    , "x LSHIFT 2 -> f"
    , "y RSHIFT 2 -> g"
    , "NOT x -> h"
    , "NOT y -> i"
    , "1 AND x -> z"
    , "y -> q"
    ]

exampleWires :: [Text]
exampleWires = map (\c -> T.pack [c]) "defghixyzq"

unit_2015_07_booklet :: IO ()
unit_2015_07_booklet =
    map (snd . value (booklet exampleInput)) exampleWires
        @?= [ 72
            , 507
            , 492
            , 114
            , 65412
            , 65079
            , 123
            , 456
            , 1
            , 456
            ]
