{- |
 Module      : AOC_2021_16
 Description : Advent of code 2021 day 16
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2021_16 (solvers, readPacket, Packet (..), Content (..)) where

import Data.Bits (Bits ((.&.)))
import Data.Char qualified as C (digitToInt)
import Data.List qualified as L (foldl')
import Data.Text (Text)
import Data.Text qualified as T (lines, pack, unpack, words)

solvers :: Text -> (Text, Text)
solvers t =
    ( "NYI" -- T.pack . show . count1478 $ concatMap snd problems
    , "NYI" -- T.pack . show . sum $ map solve problems
    )

data Packet = Packet Int Content deriving (Show, Eq)

data Content
    = Literal Int
    | Operator Int [Packet]
    deriving (Show, Eq)

data Bit = Zero | One deriving (Eq, Show)

bitToInt :: Bit -> Int
bitToInt Zero = 0
bitToInt One = 1

readPacket :: Text -> Packet
readPacket = parse . hexToBin

hexToBin :: Text -> [Bit]
hexToBin = concatMap (intToBits . C.digitToInt) . T.unpack
  where
    intToBits :: Int -> [Bit]
    intToBits x = map (\y -> if y /= 0 then One else Zero) [x .&. 8, x .&. 4, x .&. 2, x .&. 1]

parse :: [Bit] -> Packet
parse bits
    | typeId == 4 = Packet version $ Literal (readLiteral rest')
    | lengthId == [Zero] = undefined
    | otherwise = Packet version $ Operator 0 undefined
  where
    (versionBits, rest) = splitAt 3 bits
    (typeIdBits, rest') = splitAt 3 rest
    (lengthId, rest'') = splitAt 1 rest'
    version = readBits 3 versionBits
    typeId = readBits 3 typeIdBits

-- Convert a sequence of n bits into an integer
readBits :: Int -> [Bit] -> Int
readBits n bits
    | n == length bits = L.foldl' (\acc b -> acc * 2 + bitToInt b) 0 bits
    | otherwise = error "Wrong length in readBits"

readLiteral :: [Bit] -> Int
readLiteral = combineGroups . toGroups
  where
    toGroups :: [Bit] -> [Int]
    toGroups (Zero : rest)
        | length trailing < 8 && all (== Zero) trailing = [readBits 4 first4]
        | otherwise = error "Bad trailing bits"
      where
        (first4, trailing) = splitAt 4 rest
    toGroups (One : rest) = readBits 4 first4 : toGroups others
      where
        (first4, others) = splitAt 4 rest
    toGroups [] = []
    combineGroups :: [Int] -> Int
    combineGroups = L.foldl' (\acc x -> acc * 16 + x) 0
