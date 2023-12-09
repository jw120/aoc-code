{- |
 Module      : AOC_2018_14
 Description : Advent of code 2018 day 14
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_14 (solvers) where

import Data.Char as C (digitToInt, isDigit)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map (deleteMax, fromList, insert, lookupMax, size, (!))
import Data.Maybe qualified as Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T (pack, unpack)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . runA . read $ T.unpack t
    , T.pack . show . runB . map C.digitToInt $ filter C.isDigit $ T.unpack t
    )

newtype Index = Index Int deriving (Enum, Eq, Num, Ord, Show)

newtype Score = Score Int deriving (Eq, Num, Ord, Show)

scoreToIndex :: Score -> Index
scoreToIndex (Score x) = Index x

data State = State
    { a :: Index -- first elf
    , b :: Index -- second elf
    , recipes :: Map Index Score
    }

instance Show State where
    show s = xs ++ " " ++ show ia ++ " " ++ show ib
      where
        n = Map.size (recipes s)
        xs = show $ [(\(Score z) -> z) (recipes s Map.! Index i) | i <- [0 .. n - 1]]
        Index ia = a s
        Index ib = b s

initialState :: State
initialState = State{a = Index 0, b = Index 1, recipes = Map.fromList [(Index 0, Score 3), (Index 1, Score 7)]}

{- | Iterate state

 >>> next initialState
 [3,7,1,0] 0 1
 >>> next $ next initialState
 [3,7,1,0,1,0] 4 3
-}
next :: State -> State
next s = s{a = a', b = b', recipes = recipes'}
  where
    newScore = recipes s Map.! a s + recipes s Map.! b s
    (newScoreTens, newScoreUnits) = splitScore newScore
    newIndex = Index . Map.size $ recipes s
    recipes'
        | newScoreTens == 0 = Map.insert newIndex newScore (recipes s)
        | otherwise = Map.insert newIndex newScoreTens $ Map.insert (newIndex + 1) newScoreUnits (recipes s)
    a' = wrapIndex (Map.size recipes') $ a s + 1 + scoreToIndex (recipes s Map.! a s)
    b' = wrapIndex (Map.size recipes') $ b s + 1 + scoreToIndex (recipes s Map.! b s)
    splitScore :: Score -> (Score, Score)
    splitScore (Score z) = (Score (z `div` 10), Score (z `mod` 10))
    wrapIndex :: Int -> Index -> Index
    wrapIndex n (Index i) = Index (i `mod` n)

-- | Part a, iterate (n + 10) times and give last 5 score
runA :: Int -> String
runA n = [(\(Score s) -> head (show s)) (r Map.! Index i) | i <- [n .. n + 9]]
  where
    r = recipes $ iterate next initialState !! (n + 8)

-- | Part b, iterate until last recipes map given sequence, return number of recipes before sequence
runB :: [Int] -> Int
runB target = adjustIndex . lastKey . dropLastRecipeIfNotMatched $ until matchTarget next initialState
  where
    n :: Index = Index $ length target
    target' = map Score target
    -- As we add one or two recipes per cycle, match on the last n and the last n ignoring the final value
    matchTarget :: State -> Bool
    matchTarget s = target' == tail (lastRecipes s) || target' == init (lastRecipes s)
    lastRecipes :: State -> [Score]
    lastRecipes s = [recipes s Map.! i | i <- [lastKey s - n .. lastKey s], i > 0]
    lastKey :: State -> Index
    lastKey s = fst . Maybe.fromJust . Map.lookupMax $ recipes s
    adjustIndex :: Index -> Int
    adjustIndex (Index i) = i - length target + 1
    dropLastRecipeIfNotMatched :: State -> State
    dropLastRecipeIfNotMatched s
        | tail (lastRecipes s) == target' = s
        | init (lastRecipes s) == target' = s{recipes = Map.deleteMax (recipes s)}
        | otherwise = error "Not a match"
