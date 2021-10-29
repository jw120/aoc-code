{- |
 Module      : AOC_2015_17
 Description : Advent of code 2015 day 17
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_17 (solvers, solutions, ways) where

import Data.Text (Text)
import Data.Text qualified as T (lines, pack, unpack)
import Debug.Trace (trace)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ ways volume containers
    , T.pack . show $ length fewestContainerSolutions
    )
  where
    volume = 150
    containers = map (read . T.unpack) $ T.lines t
    possibleSolutions = solutions volume containers
    fewestContainers = minimum $ map length possibleSolutions
    fewestContainerSolutions = filter ((==) fewestContainers . length) possibleSolutions

ways :: Int -> [Int] -> Int
ways 0 _ = 1
ways n (x : xs)
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = ways (n - x) xs + ways n xs
ways _ [] = 0

solutions :: Int -> [Int] -> [[Int]]
solutions 0 _ = [[]]
solutions n (x : xs)
    | n < 0 = []
    | n == 0 = [[]]
    | n >= x = [x : w | w <- solutions (n - x) xs] ++ solutions n xs
    | otherwise = solutions n xs
solutions _ [] = []
