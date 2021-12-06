module AOC_2021_04_Test where

import Control.Applicative (many)
import Data.Text (Text)
import Test.Tasty.HUnit ((@?=))
import Text.Megaparsec.Char qualified as MC (space)

import AOC_2021_04 (pCard, score)
import Utilities (Parser, pUnsignedInt, parseOrStop)

test1 :: Text
test1 = " 22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19"

test1_numbers :: [Int]
test1_numbers = [22, 13, 17, 11, 0, 8, 2, 23, 4, 24, 21, 9, 14, 16, 7, 6, 10, 3, 18, 5, 1, 12, 20, 15, 19]

pTest :: Parser [Int]
pTest = MC.space *> many pUnsignedInt

unit_2021_04_parse1 :: IO ()
unit_2021_04_parse1 =
    parseOrStop pTest test1
        @?= test1_numbers

unit_2021_04_parse2 :: IO ()
unit_2021_04_parse2 = score (parseOrStop pCard test1) @?= sum test1_numbers
