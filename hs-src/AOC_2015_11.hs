{- |
 Module      : AOC_2015_XX
 Description : Advent of code 2015 day XX
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_11 (solvers, increment, nextValid, valid) where

--import Data.List qualified as L (foldl')

import Data.Char qualified as C (chr, ord)
import Data.Text (Text)

import Data.Text qualified as T (pack, unpack)

-- import Utilities (Parser, pSymbol, lexeme, pUnsignedInt, parseOrStop, ($>), (<|>))

solvers :: (Text -> Text, Text -> Text)
solvers =
    ( T.pack . nextValid . T.unpack
    , T.pack . nextValid . increment . nextValid . T.unpack
    )

increment :: String -> String
increment = incrementFromEnd 0
  where
    incrementFromEnd i s =
        let focusChar = s !! (length s - 1 - i)
         in case focusChar of
                'z' -> incrementFromEnd (i + 1) (replaceFromEnd i 'a' s)
                c -> replaceFromEnd i (C.chr (C.ord c + 1)) s
    replaceFromEnd i c s = take (length s - 1 - i) s ++ [c] ++ drop (length s - i) s

valid :: String -> Bool
valid s = hasIncreasingTriple && not hasConfusingLetter && hasTwoPairs
  where
    hasIncreasingTriple = or $ zipWith3 isAscending s (drop 1 s) (drop 2 s)
    hasConfusingLetter = 'i' `elem` s || 'o' `elem` s || 'l' `elem` s
    hasTwoPairs = numPairs > 1 && not (numPairs == 2 && hasTriple)
    numPairs = length . filter (uncurry (==)) $ zip s (drop 1 s)
    hasTriple = or $ zipWith3 (\a b c -> a == b && b == c) s (drop 1 s) (drop 2 s)
    isAscending a b c = (C.ord b == 1 + C.ord a) && (C.ord c == 1 + C.ord b)

nextValid :: String -> String
nextValid s
    | valid s = s
    | otherwise = nextValid (increment s)