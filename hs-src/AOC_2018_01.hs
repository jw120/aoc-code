{- |
 Module      : AOC_2018_01
 Description : Advent of code 2018 day 1
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_01 (solvers) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T (lines, pack)

import Utilities (pSignedInt, parseOrStop)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ sum numbers
    , T.pack . show . firstRepeat . scanl (+) 0 $ cycle numbers
    )
  where
    numbers = map num $ T.lines t

-- | Read an integer with a +/-
num :: Text -> Int
num = parseOrStop pSignedInt

-- | Return first repeated value in a (potentially infinite) list
firstRepeat :: Ord x => [x] -> x
firstRepeat = firstRepeat' Set.empty
  where
    firstRepeat' :: Ord y => Set y -> [y] -> y
    firstRepeat' visited (x : xs)
        | x `Set.member` visited = x
        | otherwise = firstRepeat' (x `Set.insert` visited) xs
    firstRepeat' _ [] = error "Unexpected empty list in firstRepeat"
