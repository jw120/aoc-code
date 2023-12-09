{- |
 Module      : AOC_2021_01
 Description : Advent of code 2021 day 1
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2021_01 (solvers) where

import Data.Text (Text)
import Data.Text qualified as T (lines, pack)

import Utilities (pUnsignedInt, parseOrStop)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ numIncreases numbers
    , T.pack . show . numIncreases $ window numbers
    )
  where
    numbers = map (parseOrStop pUnsignedInt) $ T.lines t

numIncreases :: [Int] -> Int
numIncreases xs@(_ : xt) = length . filter id $ zipWith (<) xs xt
numIncreases [] = 0

window :: [Int] -> [Int]
window (x : y : zs) = zipWith3 (\a b c -> a + b + c) (x : y : zs) (y : zs) zs
window _ = []
