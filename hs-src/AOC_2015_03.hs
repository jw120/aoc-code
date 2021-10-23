{- |
 Module      : AOC_2015_03
 Description : Advent of code 2015 day 3
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_03 (solvers, houses, robotHouses, route) where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.List (foldl')
import Data.Set (Set)
import Data.Set qualified as Set (insert, singleton, size, union)
import Data.Text (Text)
import Data.Text qualified as T (pack)
import Text.Megaparsec (eof, many)
import Text.Megaparsec.Char (char)

import Utilities (parseOrStop)

solvers :: (Text -> Text, Text -> Text)
solvers =
    ( T.pack . show . houses . route
    , T.pack . show . robotHouses . route
    )

data Move = North | East | South | West

route :: Text -> [Move]
route = parseOrStop pRoute
  where
    pRoute = many pMove <* eof
    pMove = (char '^' $> North) <|> (char '>' $> East) <|> (char 'v' $> South) <|> (char '<' $> West)

houses :: [Move] -> Int
houses = Set.size . visit

robotHouses :: [Move] -> Int
robotHouses moves = Set.size $ Set.union santa robot
  where
    labelledMoves = zip [0 :: Int ..] moves
    santa = visit [m | (i, m) <- labelledMoves, odd i]
    robot = visit [m | (i, m) <- labelledMoves, even i]

visit :: [Move] -> Set (Int, Int)
visit = snd . foldl' move ((0, 0), Set.singleton (0, 0))
  where
    move :: ((Int, Int), Set (Int, Int)) -> Move -> ((Int, Int), Set (Int, Int))
    move ((x, y), visited) mv = ((x', y'), Set.insert (x', y') visited)
      where
        (x', y') = case mv of
            North -> (x, y + 1)
            East -> (x + 1, y)
            South -> (x, y - 1)
            West -> (x - 1, y)
