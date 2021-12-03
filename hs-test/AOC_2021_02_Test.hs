module AOC_2021_02_Test where

import Data.Text (Text)
import Test.Tasty.HUnit ((@?=))

import AOC_2021_02 (Command (..), applyCommand, applyCommand', pCommand)
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
unit_2021_02_example1 = foldr applyCommand (0, 0) (map (parseOrStop pCommand) test1) @?= (15, 10)

unit_2021_02_example2 :: IO ()
unit_2021_02_example2 =
    let (horiz, depth, aim) = foldr applyCommand' (0, 0, 0) (map (parseOrStop pCommand) test1)
     in (horiz, depth, aim) @?= (15, 60, 10)

unit_2021_02_example3 :: IO ()
unit_2021_02_example3 =
    [ applyCommand' (Forward 5) (0, 0, 0)
    , applyCommand' (Down 5) (5, 0, 0)
    , applyCommand' (Forward 8) (5, 0, 5)
    , applyCommand' (Up 3) (13, 40, 5)
    , applyCommand' (Down 8) (13, 40, 2)
    , applyCommand' (Forward 2) (13, 40, 10)
    ]
        @?= [ (5, 0, 0)
            , (5, 0, 5)
            , (13, 40, 5)
            , (13, 40, 2)
            , (13, 40, 10)
            , (15, 60, 10)
            ]