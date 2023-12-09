module AOC_2021_02_Test where

import Data.List qualified as L (foldl')
import Data.Text (Text)
import Test.Tasty.HUnit ((@?=))

import AOC_2021_02 (applyCommand, applyCommand', pCommand)
import Utilities (parseOrStop)

test1 :: [Text]
test1 =
    [ "forward 5"
    , "down 5"
    , "forward 8"
    , "up 3"
    , "down 8"
    , "forward 2"
    ]

unit_2021_02_example1 :: IO ()
unit_2021_02_example1 = L.foldl' applyCommand (0, 0) (map (parseOrStop pCommand) test1) @?= (15, 10)

unit_2021_02_example2 :: IO ()
unit_2021_02_example2 =
    let (horiz, depth, aim) = L.foldl' applyCommand' (0, 0, 0) (map (parseOrStop pCommand) test1)
     in (horiz, depth, aim) @?= (15, 60, 10)