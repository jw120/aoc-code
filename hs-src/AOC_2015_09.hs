{- |
 Module      : AOC_2015_09
 Description : Advent of code 2015 day 9
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_09 (makeDistance, solvers, shortestPath, Extreme (..)) where

--import Data.Functor (($>))
--import Data.List qualified as L (foldl')

import Data.List qualified as L (permutations)
import Data.Map.Lazy qualified as Map (fromList, lookup)
import Data.Set qualified as Set (fromList, toList)
import Data.Text (Text)
import Data.Text qualified as T (lines, pack, unpack)
import Text.Megaparsec as M (some)
import Text.Megaparsec.Char qualified as MC (letterChar, string)

import Utilities (Parser, pUnsignedInt, parseOrStop)

data Extreme = Max | Min

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ solve Min ls
    , T.pack . show $ solve Max ls
    )
  where
    ls = T.lines t

solve :: Extreme -> [Text] -> Int
solve x ts = shortestPath x (makeDistance distances) cities
  where
    distances = map (parseOrStop pDist) ts
    allCities = map (fst . fst) distances ++ map (snd . fst) distances
    cities = Set.toList $ Set.fromList allCities

pDist :: Parser ((Text, Text), Int)
pDist = do
    a <- T.pack <$> M.some MC.letterChar
    _ <- MC.string " to "
    b <- T.pack <$> M.some MC.letterChar
    _ <- MC.string " = "
    d <- pUnsignedInt
    return ((a, b), d)

shortestPath :: Extreme -> (x -> x -> Int) -> [x] -> Int
shortestPath x distance locations = extreme distances
  where
    extreme = case x of
        Max -> maximum
        Min -> minimum
    routes = L.permutations locations
    distances = map totalDistance routes
    totalDistance (a : b : cs) = distance a b + totalDistance (b : cs)
    totalDistance _ = 0

makeDistance :: [((Text, Text), Int)] -> (Text -> Text -> Int)
makeDistance distances = findCities
  where
    distanceMap = Map.fromList distances
    findCities a b = case Map.lookup (a, b) distanceMap of
        Just d -> d
        Nothing -> case Map.lookup (b, a) distanceMap of
            Just d -> d
            Nothing -> error . T.unpack $ "Unknown route " <> a <> " to " <> b
