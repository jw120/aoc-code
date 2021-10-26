module AOC_2015_12_Test where

import AOC_2015_12 (JSON (..), json, stripRed, sumNumbers)

import Data.Text (Text)
import Test.Tasty.HUnit ((@?=))

examples :: [Text]
examples =
    [ "\"abc\""
    , "12"
    , "[1,2,3]"
    , "{\"a\":2,\"b\":4}"
    , "[[[3]]]"
    , "{\"a\":{\"b\":4},\"c\":-1}"
    , "{\"a\":[-1,1]}"
    , "[-1,{\"a\":1}]"
    , "[]"
    , "{}"
    ]

examplesRed :: [Text]
examplesRed =
    [ "[1,2,3]"
    , "[1,{\"c\":\"red\",\"b\":2},3]"
    , "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}"
    , "[1,\"red\",5]"
    ]

unit_2015_12_json :: IO ()
unit_2015_12_json = json "[1,2]" @?= JSONArray [JSONNumber 1, JSONNumber 2]

unit_2015_12_sumNumbers :: IO ()
unit_2015_12_sumNumbers = map (sumNumbers . json) examples @?= [0, 12, 6, 6, 3, 3, 0, 0, 0, 0]

unit_2015_12_sumNumbersRed :: IO ()
unit_2015_12_sumNumbersRed = map (sumNumbers . stripRed . json) examplesRed @?= [6, 4, 0, 6]