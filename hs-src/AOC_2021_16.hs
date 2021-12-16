{- |
 Module      : AOC_2021_16
 Description : Advent of code 2021 day 16
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2021_16 (solvers) where

import Data.Char qualified as C (digitToInt)

--import Data.List qualified as L (foldl')
--import Data.Map (Map)
--import Data.Map qualified as Map (elems, empty, filter, fromList, insertWith, keys, toList, (!))
--import Data.Maybe qualified as Maybe (mapMaybe)
--import Data.Set (Set)
--import Data.Set qualified as Set (fromList)

import Data.Semigroup qualified as Int
import Data.Text (Text)

import Data.Text qualified as T (lines, pack, splitOn, unpack, words)

solvers :: Text -> (Text, Text)
solvers t =
    ( "NYI" -- T.pack . show . count1478 $ concatMap snd problems
    , "NYI" -- T.pack . show . sum $ map solve problems
    )

data Packet = Packet Int Content

data Content
    = Literal Int
    | Operator Int [Packet]

data Bit = Zero | One

hexToBin :: Text -> [Bit]
hexToBin = concatMap (intToBits . C.digitToInt) . T.unpack
  where
    intToBits x = map (\y -> if y then One else Zero) [x .&. 8, x .&. 4, x .&. 2, x .&. 1]

parse :: [Bit] -> Packet
parse bits
    | id == 4 = Packet (read3 version) $ Literal (readLiteral rest')
    | otherwise = error "Unknown"
  where
    (version, rest) = splitAt 3 bits
    (id, rest') = splitAt 3 rest

read3 :: [Bit] -> Int
read3 [b2, b1, b0] = b b2 * 4 + b b1 * 2 + b b0
  where
    b Zero = 0
    b One = 1
read3 _ = error "Expected 3 bits"

readLiteral :: [Bit] -> Int
readLiteral = combineGroups . toGroups
  where
    toGroups :: [Bit] -> [Int]
    toGroups bits
        | length bits < 5 && all (== Z) bits = []
        | length bits < 5 = error "Bad trailing bits"
        | Z : xs