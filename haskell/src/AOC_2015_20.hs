{- |
 Module      : AOC_2015_20
 Description : Advent of code 2015 day 20
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental

https://www.geeksforgeeks.org/sum-factors-number/
https://www.amansmathsblogs.com/factors-formula-how-to-find-sum-of-factors-of-composite-numbers/
-}
module AOC_2015_20 (presents, solvers) where

-- import Data.Map.Lazy (Map)
-- import Data.Map.Lazy qualified as Map (fromList, empty, insert, insertWith, (!))
--import Data.List qualified as L (foldl')
import Data.Text (Text)
import Data.Text qualified as T (pack, unpack)

--import Text.Megaparsec qualified as M (some, sepBy)
--import Text.Megaparsec.Char qualified as MC (char, string, letterChar)

-- import Utilities (Parser, pSymbol, lexeme, pUnsignedInt, parseOrStop, ($>), (<|>))

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ lowestWith number
    , "NYI"
    )
  where
    number = read . T.unpack $ t

-- Lowest house number with at least as many presents as the input
lowestWith :: Int -> Int
lowestWith target = go 0
  where
    go :: Int -> Int
    go n
        | presents n >= target = n
        | otherwise = go (n + 1)

-- Number of presents for house numbers
presents :: Int -> Int
presents = (* 10) . sum . factors
  where
    factors x = filter (\y -> x `mod` y == 0) [1 .. x]
