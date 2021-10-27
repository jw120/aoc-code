{- |
 Module      : AOC_2015_13
 Description : Advent of code 2015 day 13
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_13 (solvers) where

--import Data.Functor (($>))
--import Data.List qualified as L (foldl')

import Data.List qualified as L (foldl', permutations)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map (fromList, unions, (!))
import Data.Set qualified as Set (fromList, toList)
import Data.Text (Text)
import Data.Text qualified as T (lines, pack)
import Text.Megaparsec as M (some)
import Text.Megaparsec.Char qualified as MC (char, letterChar, string)

import Utilities (Parser, pUnsignedInt, parseOrStop, ($>), (<|>))

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ bestSeating happinessValues names
    , T.pack . show $ bestSeating meHappinessValues meNames
    )
  where
    happinessRecords = map (parseOrStop pHappy) $ T.lines t
    happinessValues = Map.fromList $ happinessRecords
    names = Set.toList $ Set.fromList $ map (fst . fst) happinessRecords
    me = "Me"
    meNames = me : names
    meHappiness = Map.fromList $ map (\n -> ((me, n), 0)) names
    meHappiness' = Map.fromList $ map (\n -> ((n, me), 0)) names
    meHappinessValues = Map.unions [happinessValues, meHappiness, meHappiness']

pHappy :: Parser ((Text, Text), Int)
pHappy = do
    a <- T.pack <$> M.some MC.letterChar
    _ <- MC.string " would "
    negative <- (MC.string "lose " $> True) <|> (MC.string "gain " $> False)
    d <- pUnsignedInt
    _ <- MC.string "happiness units by sitting next to "
    b <- T.pack <$> M.some MC.letterChar
    _ <- MC.char '.'
    return ((a, b), if negative then - d else d)

bestSeating :: Map (Text, Text) Int -> [Text] -> Int
bestSeating happinessValues names = maximum scores
  where
    seatings = cyclicPermutations names
    scores = map (circleScore happinessValues) seatings

cyclicPermutations :: [x] -> [[x]]
cyclicPermutations (x : xs) = map (x :) $ L.permutations xs
cyclicPermutations [] = error "Unexpected empty list"

circleScore :: Map (Text, Text) Int -> [Text] -> Int
circleScore m zs@(z : _) = L.foldl' addPairScore 0 $ zip zs' (drop 1 zs')
  where
    zs' = zs ++ [z]
    addPairScore :: Int -> (Text, Text) -> Int
    addPairScore acc (a, b) = acc + m Map.! (a, b) + m Map.! (b, a)
circleScore _ [] = error "Empty table"
