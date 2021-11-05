{- |
 Module      : AOC_2018_25
 Description : Advent of code 2018 day 25
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_25 (solvers) where

import Data.List qualified as L (foldl', partition)
import Data.Set (Set)
import Data.Set qualified as Set (filter, insert, null, unions)
import Data.Text (Text)
import Data.Text qualified as T (lines, pack, splitOn, unpack)
import Data.Text.IO qualified as TIO ()
import Utilities (readSignedDecimal)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show . length $ allocatePoints points
    , "Not required"
    )
  where
    points = map readPoint $ T.lines t

type Point = (Int, Int, Int, Int)

type Constellation = Set Point

readPoint :: Text -> Point
readPoint t = case map readSignedDecimal $ T.splitOn "," t of
    [p1, p2, p3, p4] -> (p1, p2, p3, p4)
    _ -> error . T.unpack $ "Bad point " <> t

nearPoint :: Point -> Point -> Bool
nearPoint (p1, p2, p3, p4) (q1, q2, q3, q4) = d <= 3
  where
    d = abs (p1 - q1) + abs (p2 - q2) + abs (p3 - q3) + abs (p4 - q4)

nearConstellation :: Point -> Constellation -> Bool
nearConstellation p = not . Set.null . Set.filter (nearPoint p)

addPoint :: [Constellation] -> Point -> [Constellation]
addPoint cs p = Set.insert p (Set.unions near) : far
  where
    (near, far) = L.partition (nearConstellation p) cs

-- | Allocate points into constellations
allocatePoints :: [Point] -> [Constellation]
allocatePoints = L.foldl' addPoint []
