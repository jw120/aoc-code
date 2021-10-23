module AOC_2015_01 (solve) where

import Data.List (foldl', scanl')

countFloors :: String -> Int
countFloors = foldl' move 0

{- | Find first visit to basement

 >>> firstVisit ")"
 1
 >>> firstVisit "()())"
 5
-}
firstVisit :: String -> Int
firstVisit = length . takeWhile (/= (- 1)) . scanl' move 0

move :: Int -> Char -> Int
move i '(' = i + 1
move i ')' = i - 1
move _ c = error $ "Unexpected character" ++ [c]

solve :: String -> (String, String)
solve s = (show $ countFloors s, show $ firstVisit s)
