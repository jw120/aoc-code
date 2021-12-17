{-# LANGUAGE TypeFamilies #-}

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
import Data.Void (Void)
import Text.Megaparsec qualified as M (Parsec, eof, errorBundlePretty, many, optional, parse, try)
import Text.Megaparsec.Char qualified as MC (char)

import Utilities (($>), (<|>))

type Bit = Char
type Parser = M.Parsec Void [Bit]

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

readPacket :: Text -> Packet
readPacket t = case M.parse pFullPacket "" (hexToBits t) of
    Left bundle -> error (M.errorBundlePretty bundle)
    Right packet -> packet

hexToBits :: Text -> [Bit]
hexToBits = concatMap (intToBits . C.digitToInt) . T.unpack
  where
    intToBits :: Int -> [Bit]
    intToBits x = map (\y -> if y /= 0 then '1' else '0') [x .&. 8, x .&. 4, x .&. 2, x .&. 1]

-- Parse top-level packet with optional trailing zeroes and eof
pFullPacket :: Parser Packet
pFullPacket = do
    packet <- pPacket
    _ <- exactly 7 $ M.optional pZero
    _ <- M.eof
    return packet

-- 110 100 10111 11110 00101000
-- VVV TTT AAAAA BBBBB CCCCC

-- Parse a normal packet
pPacket :: Parser Packet
pPacket = Packet <$> pBits 3 <*> (M.try pLiteral <|> pOperator)

pLiteral :: Parser Content
pLiteral = do
    _id <- pOne *> pZero *> pZero
    blocks <- M.many (pOne *> pBits 4)
    finalBlock <- pZero *> pBits 4
    return . Literal $ combineBlocks (blocks ++ [finalBlock])
  where
    combineBlocks :: [Int] -> Int
    combineBlocks = L.foldl' (\acc x -> acc * 16 + x) 0

pOperator :: Parser Content
pOperator = pNumber
  where
    pLength = undefined
    -- A number of sub-packets
    pNumber = do
        typeId <- pBits 3
        _ <- pOne
        numPackets <- pBits 11
        packets <- exactly numPackets pPacket
        return $ Operator typeId packets

-- Parse n bits as an integer
pBits :: Int -> Parser Int
pBits = go 0
  where
    go acc 0 = return acc
    go acc n = do
        b <- pBit
        go (acc * 2 + b) (n - 1)

-- Parse a single bit
pBit :: Parser Int
pBit = pOne <|> pZero

pOne :: Parser Int
pOne = MC.char '1' $> 1

pZero :: Parser Int
pZero = MC.char '0' $> 0

-- apply a parser exactly x times and collect results in a list
exactly :: Int -> Parser a -> Parser [a]
exactly n p
    | n <= 0 = return []
    | otherwise = do
        x <- p
        xs <- exactly (n - 1) p
        return $ x : xs

{-

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
    (versionBits, afterVersion) = splitAt 3 bits
    (typeIdBits, afterTypeId) = splitAt 3 afterVersion
    (lengthId, afterLengthId) = splitAt 1 afterTypeId
    (subPacketsLengthBits, afterSubPacketsLength = splitAt 15 afterLengthId
    version = readBits 3 versionBits
    typeId = readBits 3 typeIdBits
    subPacketsLength = readBits 15 subPacketsLengthBits

-- 001  110 0  000000000011011  110 100 01010  010 100 10001 001000 000000
-- VVV  TTT I  LLLLLLLLLLLLLLL  AAA AAA AAAAA  BBB BBB BBBBB BBBBB

-- Convert a sequence of n bits into an integer
readBits :: Int -> [Bit] -> Int
readBits n bits
    | n == length bits = L.foldl' (\acc b -> acc * 2 + bitToInt b) 0 bits
    | otherwise = error "Wrong length in readBits"

pLiteral :: [Bit] -> Int
pLiteral = combineGroups . toGroups
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

-}