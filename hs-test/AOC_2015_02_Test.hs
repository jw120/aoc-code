module AOC_2015_02_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2015_02 (box, paper, ribbon)

unit_2015_02_paper_1 :: IO ()
unit_2015_02_paper_1 = (paper . box) "2x3x4" @?= 58

unit_2015_02_paper_2 :: IO ()
unit_2015_02_paper_2 = (paper . box) "1x1x10" @?= 43

unit_2015_02_ribbon_1 :: IO ()
unit_2015_02_ribbon_1 = (ribbon . box) "2x3x4" @?= 34

unit_2015_02_ribbon_2 :: IO ()
unit_2015_02_ribbon_2 = (ribbon . box) "1x1x10" @?= 14
