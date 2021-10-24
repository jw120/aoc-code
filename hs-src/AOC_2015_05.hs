{- |
 Module      : AOC_2015_05
 Description : Advent of code 2015 day 5
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_05 (solvers, niceOne, niceTwo) where

import Data.Text (Text)
import Data.Text qualified as T (drop, filter, isInfixOf, length, lines, pack, zip)

solvers :: (Text -> Text, Text -> Text)
solvers =
    ( T.pack . show . length . filter id . map niceOne . T.lines
    , T.pack . show . length . filter id . map niceTwo . T.lines
    )

niceOne :: Text -> Bool
niceOne t = numVowels >= 3 && hasDoubleLetter && not hasBadPair
  where
    numVowels = T.length $ T.filter (`elem` ("aeiou" :: [Char])) t
    hasDoubleLetter = any (uncurry (==)) $ T.zip t (T.drop 1 t)
    hasBadPair =
        "ab" `T.isInfixOf` t
            || "cd" `T.isInfixOf` t
            || "pq" `T.isInfixOf` t
            || "xy" `T.isInfixOf` t

niceTwo :: Text -> Bool
niceTwo t = not (null repeats) && hasRepeatedBetween
  where
    pairs = zip [(0 :: Int) ..] $ T.zip t (T.drop 1 t)
    repeats = [(i, j) | (i, (a, b)) <- pairs, (j, (c, d)) <- pairs, j > i + 1, a == c, b == d]
    hasRepeatedBetween = any (uncurry (==)) $ T.zip t (T.drop 2 t)
