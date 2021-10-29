{- |
 Module      : AOC_2018_04
 Description : Advent of code 2018 day 4
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_05 where

import Data.Char qualified as C (isLower, toLower)
import Data.List qualified as L (nub, sort)
import Data.Text (Text)
import Data.Text qualified as T (filter, foldr, map, pack, unpack)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show . length $ react t
    , T.pack . show $ shortest t
    )

-- | Are the two character reactive (same letter, opposite case)
reactive :: Char -> Char -> Bool
reactive x y = C.toLower x == C.toLower y && C.isLower x == not (C.isLower y)

-- | Run reaction on a string.  Single pass solution borrowed from https://github.com/mstksg/advent-of-code-2018
react :: Text -> String
react = T.foldr f []
  where
    f :: Char -> String -> String
    f c [] = [c]
    f c (x : xs)
        | reactive c x = xs
        | otherwise = c : x : xs

-- | Remove one character (upper and lower) and then react
reactWithout :: Text -> Char -> String
reactWithout s x = react $ T.filter (\c -> C.toLower c /= C.toLower x) s

-- | Part b - return shortest string after reacting following removal of one letter
shortest :: Text -> Int
shortest s =
    minimum . map (length . reactWithout s) . L.nub . L.sort . T.unpack $ T.map C.toLower s
