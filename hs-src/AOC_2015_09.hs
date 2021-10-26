{- |
 Module      : AOC_2015_09
 Description : Advent of code 2015 day 9
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_09 (makeDistance, solvers, shortestPath) where

--import Data.Functor (($>))
--import Data.List qualified as L (foldl')

import Data.List qualified as L (permutations, sortOn)
import Data.Map.Lazy qualified as Map (fromList, lookup)
import Data.Set qualified as Set (fromList, toList)
import Data.Text (Text)
import Data.Text qualified as T (lines, pack, unpack)
import Debug.Trace (trace)
import Text.Megaparsec as M (some)
import Text.Megaparsec.Char qualified as MC (letterChar, string)

import Utilities (Parser, lexeme, pSymbol, pUnsignedInt, parseOrStop, ($>), (<|>))

solvers :: (Text -> Text, Text -> Text)
solvers =
    ( T.pack . show . partA . T.lines
    , const "NYI"
    )

partA :: [Text] -> Int
partA ts = trace (show cities) $ shortestPath (makeDistance distances) cities
  where
    distances = trace (show ts) $ map (parseOrStop pDist) ts
    cities = trace (show distances) $ Set.toList . Set.fromList $ map (fst . fst) distances

pDist :: Parser ((Text, Text), Int)
pDist = do
    a <- T.pack <$> M.some MC.letterChar
    _ <- MC.string " to "
    b <- T.pack <$> M.some MC.letterChar
    _ <- MC.string " = "
    d <- pUnsignedInt
    return ((a, b), d)

shortestPath :: Show x => (x -> x -> Int) -> [x] -> Int
shortestPath distance locations = trace traceText $ minimum distances
  where
    traceText = unlines . map show . L.sortOn fst $ zip distances routes
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
