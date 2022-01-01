module AOC_2021_24_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2021_24 (pInstruction)

import Data.Text (Text)
import Data.Text qualified as T (pack, unpack)

import Utilities (parseOrStop)

unit_2021_24_alu_parse1 :: IO ()
unit_2021_24_alu_parse1 = show (parseOrStop pInstruction (head program1)) @?= T.unpack (head program1)

unit_2021_24_alu_parse2 :: IO ()
unit_2021_24_alu_parse2 = map (T.pack . show . parseOrStop pInstruction) program1 @?= program1

program1 :: [Text]
program1 =
    [ "inp w"
    , "mul x 0"
    , "add x z"
    , "mod x 26"
    , "div z 26"
    , "add x -7"
    , "eql x w"
    , "eql x 0"
    , "mul y 0"
    , "add y 25"
    , "mul y x"
    , "add y 1"
    , "mul z y"
    , "mul y 0"
    , "add y w"
    , "add y 3"
    , "mul y x"
    , "add z y"
    ]