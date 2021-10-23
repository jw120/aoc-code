module Main (main) where

import AOC_2015_01 qualified (solve)

main :: IO ()
main = do
    input <- "../aoc-data/input/2015_01.txt"
    let (output1, output2) = AOC_2015_01.solve input
    putStrLn output1
    putStrLn output2
