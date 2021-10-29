{- |
 Module      : AOC_2015_09
 Description : Advent of code 2015 day 9
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_09 (solvers) where

import Data.Map (Map)
import Data.Map qualified as Map (elems, empty, findMax, findMin, insert, insertWith, singleton, toList, (!))
import Data.Text (Text)
import Data.Text qualified as T (pack)

solvers :: Text -> (Text, Text)
solvers _ =
    ( T.pack . show $ highScore 431 70950
    , T.pack . show $ highScore 431 7095000
    )

newtype MarbleIndex = MarbleIndex Int deriving (Eq, Ord)

newtype Player = Player Int deriving (Eq, Ord)

next :: MarbleIndex -> MarbleIndex
next (MarbleIndex x) = MarbleIndex (x + 1)

instance Show MarbleIndex where
    show (MarbleIndex x) = show x

instance Show Player where
    show (Player p) = show p

data Marble = Marble
    { left :: MarbleIndex
    , right :: MarbleIndex
    , value :: Int
    }

hopLeft :: Map MarbleIndex Marble -> MarbleIndex -> MarbleIndex
hopLeft m i = left (m Map.! i)

hopRight :: Map MarbleIndex Marble -> MarbleIndex -> MarbleIndex
hopRight m i = right (m Map.! i)

data GameState = GameState
    { numPlayers :: Int
    , scores :: Map Player Int -- Players and their scores
    , marbles :: Map MarbleIndex Marble -- Indices and Marbles
    , currentMarbleIndex :: MarbleIndex -- zero-based index into the marbles list
    , nextMarbleValue :: Int
    }

-- For debugging
showMarbleMap :: MarbleIndex -> Map MarbleIndex Marble -> String
showMarbleMap c m = go lowestIndex
  where
    lowestIndex :: MarbleIndex = fst $ Map.findMin m
    go :: MarbleIndex -> String
    go i
        | r == lowestIndex = vs
        | otherwise = vs ++ " " ++ go r
      where
        v :: Int = value (m Map.! i)
        vs :: String = if i == c then "(" ++ show v ++ ")" else show v
        r :: MarbleIndex = right (m Map.! i)

instance Show GameState where
    show g =
        show (numPlayers g) ++ ": "
            ++ showMarbleMap (currentMarbleIndex g) (marbles g)
            ++ " "
            ++ show (Map.toList (scores g))

-- | Create a new game state
newGame :: Int -> GameState
newGame n =
    GameState
        { numPlayers = n
        , scores = Map.empty
        , marbles = Map.singleton z (Marble{left = z, right = z, value = 0})
        , currentMarbleIndex = z
        , nextMarbleValue = 1
        }
  where
    z = MarbleIndex 0

advance :: GameState -> GameState
advance g
    | nextMarbleValue g `mod` 23 == 0 = specialAdvance g
    | otherwise =
        g
            { marbles = newMarbles
            , currentMarbleIndex = c
            , nextMarbleValue = 1 + nextMarbleValue g
            }
  where
    -- current marble
    i :: MarbleIndex = currentMarbleIndex g
    -- one marble to the right
    r :: MarbleIndex = hopRight (marbles g) i
    marble_r :: Marble = marbles g Map.! r
    -- two marbles to the right
    rr :: MarbleIndex = right marble_r
    marble_rr :: Marble = marbles g Map.! rr
    -- new Marble
    c :: MarbleIndex = next . fst $ Map.findMax (marbles g)
    newMarbles =
        Map.insert r (marble_r{right = c})
            . Map.insert c (Marble{left = r, right = rr, value = nextMarbleValue g})
            . Map.insert rr (marble_rr{left = c})
            $ marbles g

-- Handle divisible by 23 case
specialAdvance :: GameState -> GameState
specialAdvance g =
    g
        { scores = newScores
        , marbles = newMarbles
        , currentMarbleIndex = l6
        , nextMarbleValue = 1 + nextMarbleValue g
        }
  where
    -- current marble
    i :: MarbleIndex = currentMarbleIndex g
    -- six marbles to the left
    h = hopLeft (marbles g)
    l6 :: MarbleIndex = h . h . h . h . h $ h i
    marble_l6 :: Marble = marbles g Map.! l6
    -- seven marbles to the left
    l7 :: MarbleIndex = h l6
    marble_l7 :: Marble = marbles g Map.! l7
    -- eight marbles to the left
    l8 :: MarbleIndex = h l7
    marble_l8 :: Marble = marbles g Map.! l8
    newMarbles =
        Map.insert l8 (marble_l8{right = l6})
            . Map.insert l6 (marble_l6{left = l8})
            $ marbles g
    currentPlayer = Player $ (nextMarbleValue g - 1) `mod` numPlayers g + 1
    currentScoreIncrement = nextMarbleValue g + value marble_l7
    newScores = Map.insertWith (+) currentPlayer currentScoreIncrement (scores g)

highScore :: Int -> Int -> Int
highScore players lastMarble = maximum . Map.elems $ scores finalState
  where
    finalState = iterate advance (newGame players) !! lastMarble
