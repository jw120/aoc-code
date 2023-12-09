{- |
 Module      : AOC_2018_08
 Description : Advent of code 2018 day 8
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_08 (solvers) where

import Data.Text (Text)
import Data.Text qualified as T (pack, unpack)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ sumMetadata inputNode
    , T.pack . show $ value inputNode
    )
  where
    inputNode = fst . node . map read $ words $ T.unpack t

data Node = Node [Node] [Int] deriving (Show)

-- | Simple parser for a node (no errors caught)
node :: [Int] -> (Node, [Int])
node (numChildren : numMetadata : afterHeader) = (Node cs ms, afterMetadata)
  where
    (cs, afterChildren) = rep numChildren node afterHeader
    (ms, afterMetadata) = splitAt numMetadata afterChildren
node _ = error "Bad node"

-- turn a parser into a parer for a list of n entries
rep :: Int -> ([Int] -> (a, [Int])) -> ([Int] -> ([a], [Int]))
rep 0 _ xs = ([], xs)
rep n p xs
    | n < 0 = error "negative in rep"
    | otherwise = (a1 : aN, restN)
  where
    (a1, rest1) = p xs
    (aN, restN) = rep (n - 1) p rest1

-- | Part a answer - sum of meta data in a node
sumMetadata :: Node -> Int
sumMetadata (Node cs ms) = sum ms + sum (map sumMetadata cs)

-- | Part b answer - value of a node
value :: Node -> Int
value (Node [] ms) = sum ms
value (Node cs ms) = sum $ map refer ms
  where
    childValues = map value cs
    refer :: Int -> Int
    refer c
        | c >= 1 && c <= length cs = childValues !! (c - 1)
        | otherwise = 0
