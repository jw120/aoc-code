{- |
 Module      : AOC_2015_03
 Description : Advent of code 2015 day 3
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_03 (solvers, houses, robotHouses, route) where

import Data.List qualified as L (foldl')
import Data.Set (Set)
import Data.Set qualified as Set (insert, singleton, size, union)
import Data.Text (Text)
import Data.Text qualified as T (pack)
import Text.Megaparsec qualified as M (many)
import Text.Megaparsec.Char qualified as MC (char)

import Utilities (parseOrStop, ($>), (<|>))

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ houses r
    , T.pack . show $ robotHouses r
    )
  where
    r = route t

data Move = North | East | South | West

route :: Text -> [Move]
route = parseOrStop pRoute
  where
    pRoute = M.many pMove
    pMove = (MC.char '^' $> North) <|> (MC.char '>' $> East) <|> (MC.char 'v' $> South) <|> (MC.char '<' $> West)

houses :: [Move] -> Int
houses = Set.size . visit

robotHouses :: [Move] -> Int
robotHouses moves = Set.size $ Set.union santa robot
  where
    labelledMoves = zip [0 :: Int ..] moves
    santa = visit [m | (i, m) <- labelledMoves, odd i]
    robot = visit [m | (i, m) <- labelledMoves, even i]

visit :: [Move] -> Set (Int, Int)
visit = snd . L.foldl' move ((0, 0), Set.singleton (0, 0))
  where
    move :: ((Int, Int), Set (Int, Int)) -> Move -> ((Int, Int), Set (Int, Int))
    move ((x, y), visited) mv = ((x', y'), Set.insert (x', y') visited)
      where
        (x', y') = case mv of
            North -> (x, y + 1)
            East -> (x + 1, y)
            South -> (x, y - 1)
            West -> (x - 1, y)
