{- |
 Module      : AOC_2015_16
 Description : Advent of code 2015 day 16
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_16 (solvers) where

import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map (fromList, lookup)

--import Data.List qualified as L (foldl')
import Data.Text (Text)

import Data.Text qualified as T (lines, pack)
import Text.Megaparsec qualified as M (sepBy, some)
import Text.Megaparsec.Char qualified as MC (letterChar, string)
import Utilities (Parser, pUnsignedInt, parseOrStop)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show . head1 . map sueNumber . filter (match memory) $ sues
    , T.pack . show . head1 . map sueNumber . filter (matchB memory) $ sues
    )
  where
    sues = map (parseOrStop pSue) $ T.lines t
    head1 :: Show a => [a] -> a
    head1 [x] = x
    head1 z = error $ "Expected only one answer" ++ show z

memory :: [(Text, Int)]
memory =
    map
        (parseOrStop pThing)
        [ "children: 3"
        , "cats: 7"
        , "samoyeds: 2"
        , "pomeranians: 3"
        , "akitas: 0"
        , "vizslas: 0"
        , "goldfish: 5"
        , "trees: 3"
        , "cars: 2"
        , "perfumes: 1"
        ]

data Sue = Sue
    { sueNumber :: Int
    , sueThings :: Map Text Int
    }

pSue :: Parser Sue
pSue = do
    number <- MC.string "Sue " *> pUnsignedInt <* MC.string ": "
    things <- pThing `M.sepBy` MC.string ", "
    return Sue{sueNumber = number, sueThings = Map.fromList things}

pThing :: Parser (Text, Int)
pThing = do
    label <- T.pack <$> M.some MC.letterChar
    number <- MC.string ": " *> pUnsignedInt
    return (label, number)

-- | Does this sue match the things we remembers
match :: [(Text, Int)] -> Sue -> Bool
match things sue = all match1 things
  where
    match1 :: (Text, Int) -> Bool
    match1 (label, number) = case Map.lookup label (sueThings sue) of
        Just n -> n == number
        Nothing -> True

-- | Does this sue match the things we remembers (part B version)
matchB :: [(Text, Int)] -> Sue -> Bool
matchB things sue = all match1 things
  where
    match1 :: (Text, Int) -> Bool
    match1 (label, number) = case Map.lookup label (sueThings sue) of
        Just n -> n `labelTest` number
        Nothing -> True
      where
        labelTest
            | label `elem` ["cats", "trees"] = (>)
            | label `elem` ["pomeranians", "goldfish"] = (<)
            | otherwise = (==)