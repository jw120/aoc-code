module AOC_2015_18_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2015_18 (Grid, countGrid, readGrid, stepGrid)

test :: Grid
test =
    readGrid
        6
        ".#.#.#\
        \...##.\
        \#....#\
        \..#...\
        \#.#..#\
        \####.."

unit_2015_18_grid_1 :: IO ()
unit_2015_18_grid_1 = countGrid test @?= 15

unit_2015_18_grid_2 :: IO ()
unit_2015_18_grid_2 = map countGrid (take 5 (iterate stepGrid test)) @?= [15, 11, 8, 4, 4]
