{- |
 Module      : AOC_2021_03
 Description : Advent of code 2021 day 3
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2021_03 (solvers) where

import Data.Text (Text)
import Data.Text qualified as T (lines, pack, unpack)

--import Utilities (Parser, pUnsignedInt, parseOrStop, (<|>))

data Bit = One | Zero deriving (Eq)

type BinStr = [Bit]

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ fromBinStr gamma * fromBinStr epsilon
    , T.pack . show $ fromBinStr oxygen * fromBinStr co2
    )
  where
    strings :: [BinStr] = map (readBinStr . T.unpack) $ T.lines t
    gamma :: BinStr = mcb strings
    epsilon :: BinStr = notBinStr gamma
    oxygen :: BinStr = selectMost True strings
    co2 :: BinStr = selectMost False strings

mcb :: [BinStr] -> BinStr
mcb [] = []
mcb ([] : _) = []
mcb bss = mostCommon (map head bss) : mcb (map tail bss)

mostCommon :: BinStr -> Bit
mostCommon b
    | 2 * length (filter (== One) b) >= length b = One
    | otherwise = Zero

selectMost :: Bool -> [BinStr] -> BinStr
selectMost most bs = go $ zip bs bs
  where
    go :: [(BinStr, BinStr)] -> BinStr
    go [] = error "No strings left!"
    go ((_, []) : _) = error "Ran out of bits"
    go [(b, _)] = b
    go bbs = go . map dropSndHeads $ filter filterCommon bbs
      where
        filterCommon :: (BinStr, BinStr) -> Bool
        filterCommon (_, x : _) = x == common
        filterCommon (_, []) = error "No more bits"
        common = (if most then id else notBit) . mostCommon $ map (head . snd) bbs
        dropSndHeads :: (BinStr, BinStr) -> (BinStr, BinStr)
        dropSndHeads (b, _ : xs) = (b, xs)
        dropSndHeads (_, []) = error "No more bits"

readBinStr :: String -> BinStr
readBinStr = map readBit
  where
    readBit :: Char -> Bit
    readBit '1' = One
    readBit '0' = Zero
    readBit c = error $ "Unknown bit " ++ [c]

fromBinStr :: BinStr -> Int
fromBinStr = go 0 1 . reverse
  where
    go acc base (One : xs) = go (acc + base) (base * 2) xs
    go acc base (Zero : xs) = go acc (base * 2) xs
    go acc _ [] = acc

notBinStr :: BinStr -> BinStr
notBinStr = map notBit

notBit :: Bit -> Bit
notBit Zero = One
notBit One = Zero