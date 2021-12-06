{- |
 Module      : AOC_2021_04
 Description : Advent of code 2021 day 4
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2021_04 (solvers, pCard, score) where

import Control.Applicative (many)
import Data.Array (Array)
import Data.Array qualified as A (listArray, (!), (//))
import Data.Ix qualified as Ix (range)
import Data.List qualified as L (partition)
import Data.Text (Text)
import Data.Text qualified as T (pack, split, splitOn)
import Text.Megaparsec.Char qualified as MC (space)

import Utilities (Parser, pUnsignedInt, parseOrStop)

solvers :: Text -> (Text, Text)
solvers t = case T.splitOn "\n\n" t of
    [] -> error "Invalid input"
    numbersText : cardsText -> (scoreText firstCard, scoreText lastCard)
      where
        scoreText :: Card -> Text = T.pack . show . score
        (firstCard, lastCard) = runCards cards numbers
        numbers :: [Int] = map (parseOrStop pUnsignedInt) $ T.split (== ',') numbersText
        cards :: [Card] = map (parseOrStop pCard) cardsText

runCards :: [Card] -> [Int] -> (Card, Card)
runCards cards numbers = firstAndLast $ go cards numbers []
  where
    firstAndLast :: [Card] -> (Card, Card)
    firstAndLast [] = error "Can't take first and last of empty list"
    firstAndLast [_] = error "Can't take first and last of singleton"
    firstAndLast xs = (head xs, last xs)
    go :: [Card] -> [Int] -> [Card] -> [Card]
    go [] _ completedCards = completedCards
    go _ [] _ = error "Not enough numbers"
    go activeCards (n : ns) completedCards = go activeCards' ns (completedCards ++ newCompletedCards)
      where
        (newCompletedCards, activeCards') = L.partition isComplete $ map (markNumber n) activeCards

cardSize :: Int
cardSize = 5

cardRange :: [Int]
cardRange = [1 .. cardSize]

cardBounds :: ((Int, Int), (Int, Int))
cardBounds = ((1, 1), (cardSize, cardSize))

data Card = Card
    { cardNumbers :: Array (Int, Int) Int
    , cardMarked :: Array (Int, Int) Bool
    , lastNumber :: Int
    }
    deriving (Show)

pCard :: Parser Card
pCard = mkCard <$> (MC.space *> many pUnsignedInt)
  where
    mkCard numbers =
        Card
            { cardNumbers = A.listArray cardBounds numbers
            , cardMarked = A.listArray cardBounds $ replicate (cardSize * cardSize) False
            , lastNumber = 1
            }

score :: Card -> Int
score card = lastNumber card * sum [cardNumbers card A.! i | i <- Ix.range cardBounds, not (cardMarked card A.! i)]

isComplete :: Card -> Bool
isComplete card = any rowComplete cardRange || any colComplete cardRange
  where
    rowComplete :: Int -> Bool
    rowComplete r = all (\c -> cardMarked card A.! (r, c)) cardRange
    colComplete :: Int -> Bool
    colComplete c = all (\r -> cardMarked card A.! (r, c)) cardRange

markNumber :: Int -> Card -> Card
markNumber n card = case findNumber n card of
    Just i -> card{cardMarked = cardMarked card A.// [(i, True)], lastNumber = n}
    Nothing -> card

findNumber :: Int -> Card -> Maybe (Int, Int)
findNumber n card = case filter (\ix -> cardNumbers card A.! ix == n) (Ix.range cardBounds) of
    [] -> Nothing
    x : _ -> Just x