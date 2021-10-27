{-# LANGUAGE ScopedTypeVariables #-}

module AOC_2018_06 where

import Data.Array (Array, Ix)
import Data.Array qualified as A (array, elems, range, (!))
import Data.Function qualified as Func (on)
import Data.List qualified as L (foldl', maximumBy, minimumBy, nub)
import Data.Map (Map)
import Data.Map qualified as Map (empty, insertWith, toList)
import Data.Maybe qualified as Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T (lines, pack)
import Text.Megaparsec.Char qualified as MC (char)

import Utilities (Parser, lexeme, pSignedInt, parseOrStop)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ largestArea points
    , T.pack . show $ pointsWithTotalDistWithin 10000 points
    )
  where
    points = map (parseOrStop pPoint) $ T.lines t

data Point = Point {pointX :: Int, pointY :: Int} deriving (Show, Eq, Ord, Ix)

pPoint :: Parser Point
pPoint = do
    x <- pSignedInt
    y <- lexeme (MC.char ',') *> pSignedInt
    return $ Point x y

-- | Bounding box of a set of point
boundingBox :: [Point] -> (Point, Point)
boundingBox ps = (Point (minimum xs) (minimum ys), Point (maximum xs) (maximum ys))
  where
    xs = map pointX ps
    ys = map pointY ps

-- | Part a: What is the size of the largest closest-area of the points with finite area
largestArea :: [Point] -> Int
largestArea points = maxArea
  where
    closestArray :: Array Point (Maybe Point) = closest points
    edgePointNearest :: [Point] = L.nub . Maybe.mapMaybe (closestArray A.!) $ edgePoints points
    allPointsAndCounts :: [(Point, Int)] = Map.toList . counts $ closest points
    interiorPointsAndCounts :: [(Point, Int)] = filter ((`notElem` edgePointNearest) . fst) allPointsAndCounts
    (_maxPoint, maxArea) = L.maximumBy (compare `Func.on` snd) interiorPointsAndCounts

closest :: [Point] -> Array Point (Maybe Point)
closest points = A.array pointRange [(p, closestPoint p points) | p <- A.range pointRange]
  where
    pointRange = boundingBox points

-- | Count the values in the array
counts :: Array Point (Maybe Point) -> Map Point Int
counts = L.foldl' f Map.empty . A.elems
  where
    f :: Map Point Int -> Maybe Point -> Map Point Int
    f m (Just p) = Map.insertWith (+) p 1 m
    f m Nothing = m

-- | Which of the points are on the interior
interiorPoints :: [Point] -> [Point]
interiorPoints points =
    filter
        (\p -> pointX p /= pointX p1 && pointX p /= pointX p2 && pointY p /= pointY p1 && pointY p /= pointY p1)
        points
  where
    (p1, p2) = boundingBox points

-- | Return all the points on the bounding box edge of the set of points
edgePoints :: [Point] -> [Point]
edgePoints points =
    [Point xMin y | y <- [yMin .. yMax]]
    ++ [Point x yMax | x <- [xMin .. xMax]]
        ++ [Point xMax y | y <- [yMin .. yMax]]
        ++ [Point x yMin | x <- [xMin .. xMax]]
  where
    (Point xMin yMin, Point xMax yMax) = boundingBox points

-- | Manhattan distance between two points
distance :: Point -> Point -> Int
distance (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Return the closest point if there is a unique closest
closestPoint :: Point -> [Point] -> Maybe Point
closestPoint p points
    | length allClosest == 1 = Just closestPt
    | otherwise = Nothing
  where
    allClosest = filter (\(_, d) -> d == closestDistance) pointsAndDistances
    (closestPt, closestDistance) = L.minimumBy (compare `Func.on` snd) pointsAndDistances
    pointsAndDistances = map (\q -> (q, distance p q)) points

pointsWithTotalDistWithin :: Int -> [Point] -> Int
pointsWithTotalDistWithin maxDist points = length $ filter (\p -> totalDist p <= maxDist) allFeasiblePoints
  where
    totalDist :: Point -> Int
    totalDist p = sum $ map (distance p) points
    (Point xMin yMin, Point xMax yMax) = boundingBox points
    d :: Int = 10000 `div` length points -- a heuristic on how far out we need to go
    allFeasiblePoints :: [Point]
    allFeasiblePoints = [Point x y | x <- [- xMin - d .. xMax + d], y <- [- yMin - d .. yMax + d]]
