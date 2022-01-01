module AOC_2021_24_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2021_24 (parseProgram, run, unparseProgram)

import Data.Text (Text)

--import Data.Text qualified as T (pack, unpack)

unit_2021_24_alu_parse1 :: IO ()
unit_2021_24_alu_parse1 = unparseProgram (parseProgram program1) @?= program1

unit_2021_24_alu_parse2 :: IO ()
unit_2021_24_alu_parse2 = unparseProgram (parseProgram program2) @?= program2

unit_2021_24_alu_run0 :: IO ()
unit_2021_24_alu_run0 = show (run (take 6 (parseProgram program1))) @?= "(i0,10,0,0)"

unit_2021_24_alu_run1 :: IO ()
unit_2021_24_alu_run1 = show (run (parseProgram program1)) @?= "(i0,10,0,0)"

unit_2021_24_alu_run2 :: IO ()
unit_2021_24_alu_run2 = show (run (parseProgram program2)) @?= "(i0,10,0,0)"

-- First part of problem input
program1 :: [Text]
program1 =
    [ "inp w"
    , "mul x 0"
    , "add x z"
    , "mod x 26"
    , "div z 1"
    , "add x 10"
    , "eql x w"
    , "eql x 0"
    , "mul y 0"
    , "add y 25"
    , "mul y x"
    , "add y 1"
    , "mul z y"
    , "mul y 0"
    , "add y w"
    , "add y 2"
    , "mul y x"
    , "add z y"
    ]

-- Last part of problem input
program2 :: [Text]
program2 =
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