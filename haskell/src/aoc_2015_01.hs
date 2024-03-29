{- |
 Module      : AOC_2015_01
 Description : Advent of code 2015 day 1
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_01 (solvers, firstVisit) where

import Data.List qualified as L (scanl')
import Data.Text (Text)
import Data.Text qualified as T (foldl', pack, unpack)

solvers :: Text -> (Text, Text)
solvers t = (T.pack . show $ countFloors t, T.pack . show $ firstVisit t)

countFloors :: Text -> Int
countFloors = T.foldl' move 0

firstVisit :: Text -> Int
firstVisit = length . takeWhile (/= (- 1)) . L.scanl' move 0 . T.unpack

move :: Int -> Char -> Int
move i '(' = i + 1
move i ')' = i - 1
move _ c = error $ "Unexpected character" ++ [c]
