{- |
 Module      : AOC_2021_07
 Description : Advent of code 2021 day 7
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2021_07 (solvers) where

import Data.Text (Text)
import Data.Text qualified as T (pack, split)

import Utilities (pUnsignedInt, parseOrStop)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ findMinimum (fuel1 crabs) bounds
    , T.pack . show $ findMinimum (fuel2 crabs) bounds
    )
  where
    crabs = map (parseOrStop pUnsignedInt) $ T.split (== ',') t
    bounds = (minimum crabs, maximum crabs)

findMinimum :: (Int -> Int) -> (Int, Int) -> Int
findMinimum f (xMin, xMax)
    | xMin == xMax = f xMin
    | x > xMin && f (x - 1) < f x = findMinimum f (xMin, x - 1)
    | x < xMax && f (x + 1) < f x = findMinimum f (x + 1, xMax)
    | f (x - 1) > f x && f (x + 1) > f x = f x
    | otherwise = error "Confused in findMinimum"
  where
    x = (xMin + xMax) `div` 2

fuel1 :: [Int] -> Int -> Int
fuel1 positions target = sum [abs (target - x) | x <- positions]

fuel2 :: [Int] -> Int -> Int
fuel2 positions target = sum [sumTo (abs (target - x)) | x <- positions]
  where
    sumTo n = n * (n + 1) `div` 2