{- |
 Module      : AOC_2021_05
 Description : Advent of code 2021 day 5
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2021_05 (solvers) where

import Data.List qualified as L (foldl')
import Data.Map (Map)
import Data.Map qualified as Map (empty, filter, insertWith, size)
import Data.Text (Text)
import Data.Text qualified as T (lines, pack)
import Text.Megaparsec.Char qualified as MC (char, string)

import Utilities (Parser, pUnsignedInt, parseOrStop)

data Segment
    = Horizontal (Int, Int) Int -- (xMin, xMax) y
    | Vertical Int (Int, Int) -- x (yMin, yMax)
    | DiagonalUp (Int, Int) Int -- (x y) len so (x, y), (x + 1, y + 1) ...
    | DiagonalDown (Int, Int) Int -- (x y) len so (x, y), (x + 1, y - 1)

mkSegment :: (Int, Int) -> (Int, Int) -> Segment
mkSegment (x1, y1) (x2, y2)
    | y1 == y2 = Horizontal (min x1 x2, max x1 x2) y1
    | x1 == x2 = Vertical x1 (min y1 y2, max y1 y2)
    | x1 < x2 = (if y1 < y2 then DiagonalUp else DiagonalDown) (x1, y1) (x2 - x1)
    | otherwise = (if y1 < y2 then DiagonalDown else DiagonalUp) (x2, y2) (x1 - x2)

coordList :: Segment -> [(Int, Int)]
coordList (Horizontal (x1, x2) y) = [(x, y) | x <- [x1 .. x2]]
coordList (Vertical x (y1, y2)) = [(x, y) | y <- [y1 .. y2]]
coordList (DiagonalUp (x, y) d) = [(x + i, y + i) | i <- [0 .. d]]
coordList (DiagonalDown (x, y) d) = [(x + i, y - i) | i <- [0 .. d]]

isHorizVert :: Segment -> Bool
isHorizVert (Horizontal _ _) = True
isHorizVert (Vertical _ _) = True
isHorizVert _ = False

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ multiples segmentsHV
    , T.pack . show $ multiples segments
    )
  where
    segments :: [Segment] = map (parseOrStop pSegment) $ T.lines t
    segmentsHV :: [Segment] = filter isHorizVert segments

pSegment :: Parser Segment
pSegment = do
    x1 <- pUnsignedInt
    y1 <- MC.char ',' *> pUnsignedInt
    x2 <- MC.string "-> " *> pUnsignedInt
    y2 <- MC.char ',' *> pUnsignedInt
    return $ mkSegment (x1, y1) (x2, y2)

multiples :: [Segment] -> Int
multiples = Map.size . Map.filter (> 1) . L.foldl' addCoord Map.empty . concatMap coordList
  where
    addCoord :: Map (Int, Int) Int -> (Int, Int) -> Map (Int, Int) Int
    addCoord m p = Map.insertWith (+) p 1 m
