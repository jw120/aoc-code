{- |
 Module      : AOC_2021_06
 Description : Advent of code 2021 day 6
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2021_06 (solvers, step, alive, alive', alive'') where

import Data.Array (Array)
import Data.Array qualified as A (array, (!))
import Data.Text (Text)
import Data.Text qualified as T (pack, split)

import Utilities (pUnsignedInt, parseOrStop)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ alive'' 80 initial
    , T.pack . show $ alive'' 256 initial
    )
  where
    initial = map (parseOrStop pUnsignedInt) $ T.split (== ',') t

-- Internal timer value for a fish which has reset
fishReset :: Int
fishReset = 6

-- Internal timer value for a new fish
fishNew :: Int
fishNew = 8

-- Lowest possible internal timer value for a fish before it resets
fishMin :: Int
fishMin = 0

-- Naive simulation
step :: [Int] -> [Int]
step xs = xs' ++ replicate numberNewFish fishNew
  where
    xs' = map stepFish xs
    numberNewFish = length $ filter (== 0) xs
    stepFish :: Int -> Int
    stepFish x
        | x == fishMin = fishReset
        | otherwise = x - 1

-- Recursive solution. How many descendants after n steps does fish with given timers have?
alive :: Int -> [Int] -> Int
alive n = sum . map (go n)
  where
    go :: Int -> Int -> Int
    go t i
        | t == 0 = 1
        | i == fishMin = go (t - 1) fishReset + go (t - 1) fishNew
        | otherwise = go (t - 1) (i - 1)

-- Dynamic programming version of alive
alive' :: Int -> [Int] -> Int
alive' n = sum . map (\i -> g A.! (n, i))
  where
    g :: Array (Int, Int) Int
    g = A.array ((0, fishMin), (n, fishNew)) [((t, i), f t i) | t <- [0 .. n], i <- [fishMin .. fishNew]]
    f :: Int -> Int -> Int
    f t i
        | t == 0 = 1
        | i == fishMin = g A.! (t - 1, fishReset) + g A.! (t - 1, fishNew)
        | otherwise = g A.! (t - 1, i - 1)

-- Simpler version of dynamic programming version of alive, just track the number of descendants from a fish that is about to reset
alive'' :: Int -> [Int] -> Int
alive'' n = sum . map (\i -> g A.! (n - i))
  where
    g :: Array Int Int
    g = A.array (- fishNew, n) [(t, f t) | t <- [- fishNew .. n]]
    f :: Int -> Int
    f t
        | t <= 0 = 1
        | otherwise = g A.! (t - 1 - fishReset) + g A.! (t - 1 - fishNew)
