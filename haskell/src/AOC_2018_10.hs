{- |
 Module      : AOC_2018_10
 Description : Advent of code 2018 day 10
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_10 (solvers) where

import Data.Text (Text)
import Data.Text qualified as T (lines, pack, unlines)
import Text.Megaparsec.Char qualified as MC (char, string)

import Utilities (Parser, lexeme, pSignedInt, parseOrStop)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack $ show minIndex
    , T.unlines $ showPoints minBoard
    )
  where
    (minIndex, minBoard) :: (Int, [Point]) = firstLocalMin allBoards'
    allBoards' :: [(Int, (Int, [Point]))] = zipWith (\b i -> (area b, (i, b))) allBoards [0 ..]
    allBoards :: [[Point]] = iterate (map advance) points
    points = map (parseOrStop pPoint) $ T.lines t

data Point = Point
    { position :: (Int, Int)
    , velocity :: (Int, Int)
    }
    deriving (Show)

pPoint :: Parser Point
pPoint = do
    x <- lexeme (MC.string "position=<") *> pSignedInt
    y <- lexeme (MC.char ',') *> pSignedInt
    vx <- lexeme (MC.string "> velocity=<") *> pSignedInt
    vy <- lexeme (MC.char ',') *> pSignedInt
    _ <- MC.string ">"
    return Point{position = (x, y), velocity = (vx, vy)}

boundingBox :: (Ord a, Ord b) => [(a, b)] -> ((a, b), (a, b))
boundingBox xs = ((xMin, yMin), (xMax, yMax))
  where
    xMin = minimum $ map fst xs
    xMax = maximum $ map fst xs
    yMin = minimum $ map snd xs
    yMax = maximum $ map snd xs

-- | Convert points into a grid of #s and .s suitable for printing to console
showPoints :: [Point] -> [Text]
showPoints points = [showRow r | r <- [yMin .. yMax]]
  where
    positions :: [(Int, Int)] = map position points
    ((xMin, yMin), (xMax, yMax)) = boundingBox positions
    showRow :: Int -> Text
    showRow r = T.pack [if (i, r) `elem` positions then '#' else '.' | i <- [xMin .. xMax]]

-- | Advance a point by its velocity
advance :: Point -> Point
advance p = p{position = (x + vx, y + vy)}
  where
    (x, y) = position p
    (vx, vy) = velocity p

-- | Area of the bounding box of a set of points
area :: [Point] -> Int
area points = (xMax - xMin) * (yMax - yMin)
  where
    positions :: [(Int, Int)] = map position points
    ((xMin, yMin), (xMax, yMax)) = boundingBox positions

{- | Find first local minimum of an indexed list

 >>> firstLocalMin [(3, 'A'),(2, 'B'),(1, 'C'),(2, 'D'),(0, 'E')]
 'C'
-}
firstLocalMin :: Ord x => [(x, y)] -> y
firstLocalMin ((x1, _y1) : (x2, y2) : (x3, y3) : rest)
    | x1 > x2 && x2 < x3 = y2
    | otherwise = firstLocalMin ((x2, y2) : (x3, y3) : rest)
firstLocalMin _ = error "Cannot find local minimum"
