{- |
 Module      : AOC_2018_11
 Description : Advent of code 2018 day 11
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_11 (solvers) where

import Data.Array (Array)
import Data.Array qualified as A (array, assocs, (!))
import Data.Function qualified as Func (on)
import Data.List qualified as L (maximumBy)
import Data.Maybe qualified as Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T (pack, unpack)

--import Text.Megaparsec qualified as M (some, )
--import Text.Megaparsec.Char qualified as MC (char, string, letterChar)

-- import Utilities (Parser, pSymbol, lexeme, pUnsignedInt, parseOrStop, ($>), (<|>))

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show . fst $ highestSquare serial
    , T.pack . show . fst $ highestSquareOfAnySize serial
    )
  where
    serial = read $ T.unpack t

-- Problem size
n :: Int
n = 300

-- | Extract the hundreds digit from a positive integer
hundredsDigit :: Int -> Int
hundredsDigit x = ((x - x `mod` 100) `div` 100) `mod` 10

-- | Compute power level of a cell
powerLevel :: Int -> Int -> Int -> Int
powerLevel serial x y = hundredsDigit (initialPowerLevel * rackID) - 5
  where
    rackID = x + 10
    initialPowerLevel = rackID * y + serial

-- | Part a , return the top-left coordinates of the 3x3 square with the highest powerLevel
highestSquare :: Int -> ((Int, Int), Int)
highestSquare serial = L.maximumBy (compare `Func.on` snd) boxScores
  where
    g :: Array (Int, Int) Int =
        A.array ((1, 1), (n, n)) [((x, y), powerLevel serial x y) | x <- [1 .. n], y <- [1 .. n]]
    boxScores :: [((Int, Int), Int)] =
        [((x, y), box x y) | x <- [1 .. n -2], y <- [1 .. n -2]]
    box :: Int -> Int -> Int
    box x y = sum [g A.! (x + i, y + j) | i <- [0 .. 2], j <- [0 .. 2]]

-- | Part b, Return the top-left coordinates and size of square with the highest powerLevel
highestSquareOfAnySize :: Int -> ((Int, Int, Int), Int)
highestSquareOfAnySize serial = L.maximumBy (compare `Func.on` snd) . filterJusts $ A.assocs boxScores
  where
    -- Scores of single squares
    g :: Array (Int, Int) Int =
        A.array ((1, 1), (n, n)) [((x, y), powerLevel serial x y) | x <- [1 .. n], y <- [1 .. n]]
    -- Scores of squares of given size (constructed lazily), nothing when size is out of bounds
    boxScores :: Array (Int, Int, Int) (Maybe Int) =
        A.array
            ((1, 1, 1), (n, n, n))
            [((x, y, s), box s x y) | s <- [1 .. n], x <- [1 .. n], y <- [1 .. n]]
    box :: Int -> Int -> Int -> Maybe Int
    box s x y
        | s == 1 = Just (g A.! (x, y))
        | x + s > n || y + s > n = Nothing
        | otherwise =
            Just $
                Maybe.fromJust (boxScores A.! (x, y, s - 1))
                    + sum [g A.! (i, y + s - 1) | i <- [x .. x + s - 1]]
                    + sum [g A.! (x + s - 1, j) | j <- [y .. y + s - 1]]
                    - g A.! (x + s - 1, y + s - 1)
    filterJusts :: [((Int, Int, Int), Maybe Int)] -> [((Int, Int, Int), Int)]
    filterJusts = map removeJust . filter hasJust
      where
        hasJust (_, Just _) = True
        hasJust (_, Nothing) = False
        removeJust (c, Just x) = (c, x)
        removeJust (_, Nothing) = error "Unexpected removeJust"
