{- |
 Module      : AOC_2021_04
 Description : Advent of code 2021 day 4
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2021_04 (solvers) where

import Control.Applicative (many)
import Data.Array (Array)
import Data.Array qualified as A (listArray, (!))
import Data.Ix qualified as Ix (range)
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
runCards = undefined

cardSize :: Int
cardSize = 5

cardBounds :: ((Int, Int), (Int, Int))
cardBounds = ((1, 1), (cardSize, cardSize))

data Card = Card
    { cardNumbers :: Array (Int, Int) Int
    , cardMarked :: Array (Int, Int) Bool
    , lastNumber :: Int
    }

pCard :: Parser Card
pCard = mkCard <$> many pRow
  where
    pRow = MC.space *> many pUnsignedInt
    mkCard rows =
        Card
            { cardNumbers = A.listArray cardBounds $ concat rows
            , cardMarked = A.listArray cardBounds $ repeat False
            , lastNumber = 0
            }

score :: Card -> Int
score c = lastNumber c * sum [cardNumbers c A.! i | i <- Ix.range cardBounds, (cardMark c) A.! i]
