{- |
 Module      : AOC_2015_02
 Description : Advent of code 2015 day 2
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_02 (solvers, paper, ribbon, box) where

import Data.Text (Text)
import Data.Text qualified as T (lines)
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as ML (decimal)

import Utilities (Parser, parseOrStop)

solvers :: (Text -> Int, Text -> Int)
solvers = (sum . map (paper . box) . T.lines, sum . map (ribbon . box) . T.lines)

box :: Text -> (Int, Int, Int)
box = parseOrStop pBox

pBox :: Parser (Int, Int, Int)
pBox = do
    x <- ML.decimal
    _ <- char 'x'
    y <- ML.decimal
    _ <- char 'x'
    z <- ML.decimal
    _ <- eof
    return $ sortTriple (x, y, z)

sortTriple :: (Int, Int, Int) -> (Int, Int, Int)
sortTriple (a, b, c)
    | a == smallest = let (x, y) = sortPair (b, c) in (a, x, y)
    | b == smallest = let (x, y) = sortPair (a, c) in (b, x, y)
    | otherwise = let (x, y) = sortPair (a, b) in (c, x, y)
  where
    smallest = minimum [a, b, c]
    sortPair (p, q) = if p <= q then (p, q) else (q, p)

paper :: (Int, Int, Int) -> Int
paper (l, w, h) = 2 * l * w + 2 * w * h + 2 * h * l + l * w * h `div` h

ribbon :: (Int, Int, Int) -> Int
ribbon (l, w, h) = 2 * l + 2 * w + l * w * h
