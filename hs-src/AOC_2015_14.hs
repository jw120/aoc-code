{- |
 Module      : AOC_2015_14
 Description : Advent of code 2015 day 14
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_14 (solvers, advance, pReindeer, Reindeer (..)) where

--import Data.List qualified as L (foldl')
import Data.Text (Text)

import Data.Text qualified as T (lines, pack)

import Text.Megaparsec qualified as M (some)
import Text.Megaparsec.Char qualified as MC (letterChar, string)

import Utilities (Parser, pUnsignedInt, parseOrStop)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show . maximum . map distance $ advance 2503 reindeer
    , T.pack . show . maximum . map score $ advance' 2503 reindeer
    )
  where
    reindeer = map (parseOrStop pReindeer) $ T.lines t

data Reindeer = Reindeer
    { name :: Text
    , flySpeed :: Int
    , flyTime :: Int
    , restTime :: Int
    , isFlying :: Bool
    , timeLeft :: Int
    , distance :: Int
    , score :: Int
    }

pReindeer :: Parser Reindeer
pReindeer = do
    name <- T.pack <$> M.some MC.letterChar
    flySpeed <- MC.string " can fly " *> pUnsignedInt
    flyTime <- MC.string "km/s for " *> pUnsignedInt
    restTime <- MC.string "seconds, but then must rest for " *> pUnsignedInt
    _ <- MC.string "seconds."
    return
        Reindeer
            { name = name
            , flySpeed = flySpeed
            , flyTime = flyTime
            , restTime = restTime
            , isFlying = True
            , timeLeft = flyTime
            , distance = 0
            , score = 0
            }

advance :: Int -> [Reindeer] -> [Reindeer]
advance maxTime deer
    | maxTime <= 0 = deer
    | otherwise = advance maxTime' deer'
  where
    timeIncrement = minimum $ maxTime : map timeLeft deer
    deer' = map (advanceDeer timeIncrement) deer
    maxTime' = maxTime - timeIncrement

advanceDeer :: Int -> Reindeer -> Reindeer
advanceDeer t d
    | isFlying d && t == timeLeft d = d{isFlying = False, timeLeft = restTime d, distance = distance d + flySpeed d * t}
    | isFlying d = d{timeLeft = timeLeft d - t, distance = distance d + flySpeed d * t}
    | t == timeLeft d = d{isFlying = True, timeLeft = flyTime d}
    | otherwise = d{timeLeft = timeLeft d - t}

advance' :: Int -> [Reindeer] -> [Reindeer]
advance' t deer
    | t <= 0 = deer
    | otherwise = advance' (t - 1) deer''
  where
    deer' = map (advanceDeer 1) deer
    leadingDistance = maximum $ map distance deer'
    leadingDeer = map name $ filter ((== leadingDistance) . distance) deer'
    deer'' = map scoreLeaders deer'
    scoreLeaders d
        | name d `elem` leadingDeer = d{score = score d + 1}
        | otherwise = d
