{- |
 Module      : AOC_2018_17
 Description : Advent of code 2018 day 17
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_17 (solvers) where

import Control.Monad qualified as Mon (forM_)
import Control.Monad.ST.Lazy (ST, runST)
import Data.Array (Ix)
import Data.Array.ST qualified as A (STArray, getBounds, getElems, newListArray, readArray, writeArray)
import Data.Char (isDigit)
import Data.List (foldl1')
import Data.Text (Text)
import Data.Text qualified as T (drop, head, lines, null, pack, split)
import Data.Text.IO qualified as TIO ()
import Data.Text.Read qualified as TR (decimal)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack $ show partA
    , T.pack $ show partB
    )
  where
    inputRecords = map parseInputRecord $ T.lines t
    (partA, partB) = runST $ do
        (initial, yMin) <- parseReservoir inputRecords
        final <- run initial (xStart, yMin)
        wet <- countWet final
        water <- countWater final
        return (wet, water)

data Tile = DrySand | WetSand | Water | Clay deriving (Eq, Show)

isPorous :: Tile -> Bool
isPorous DrySand = True
isPorous WetSand = True
isPorous _ = False

isWet :: Tile -> Bool
isWet WetSand = True
isWet Water = True
isWet _ = False

-- Location below the spring
xStart :: Int
xStart = 500

type Reservoir s = A.STArray s (Int, Int) Tile

data InputRecord = Horizontal (Int, Int) Int | Vertical Int (Int, Int) deriving (Show)

xRange :: InputRecord -> (Int, Int)
xRange (Horizontal xR _) = xR
xRange (Vertical x _) = (x, x)

yRange :: InputRecord -> (Int, Int)
yRange (Horizontal _ y) = (y, y)
yRange (Vertical _ yR) = yR

parseInputRecord :: Text -> InputRecord
parseInputRecord input
    | isVertical = Vertical (head numbers) (numbers !! 1, numbers !! 2)
    | otherwise = Horizontal (numbers !! 1, numbers !! 2) (head numbers)
  where
    isVertical :: Bool = T.head input == 'x'
    numbers :: [Int] = map readInt . filter (not . T.null) . T.split (not . isDigit) $ T.drop 2 input
    readInt :: Text -> Int
    readInt t = i
      where
        (i, _) = case TR.decimal t of
            Left err -> error err
            Right x -> x

parseReservoir :: [InputRecord] -> ST s (Reservoir s, Int)
parseReservoir records = do
    r <- A.newListArray ((xMin - 1, yMin), (xMax + 1, yMax)) $ replicate n DrySand
    Mon.forM_ records (addRecord r)
    return (r, yMin)
  where
    (xMin, xMax) = foldl1' joinRanges $ map xRange records
    (yMin, yMax) = foldl1' joinRanges $ map yRange records
    n = (xMax - xMin + 3) * (yMax - yMin + 1)
    joinRanges :: (Int, Int) -> (Int, Int) -> (Int, Int)
    joinRanges (m0, m1) (n0, n1) = (min m0 n0, max m1 n1)
    addRecord :: Reservoir s -> InputRecord -> ST s ()
    addRecord r (Horizontal (x0, x1) y) = Mon.forM_ [(x, y) | x <- [x0 .. x1]] (\i -> A.writeArray r i Clay)
    addRecord r (Vertical x (y0, y1)) = Mon.forM_ [(x, y) | y <- [y0 .. y1]] (\i -> A.writeArray r i Clay)

countWet :: Reservoir s -> ST s Int
countWet r = length . filter isWet <$> A.getElems r

countWater :: Reservoir s -> ST s Int
countWater r = length . filter (== Water) <$> A.getElems r

run :: Reservoir s -> (Int, Int) -> ST s (Reservoir s)
run r loc = do
    ((_xMin, _yMin), (_xMax, yMax)) <- A.getBounds r
    go yMax loc
  where
    --    go :: Int -> (Int, Int) -> ST s (Reservoir s)
    go yMax (x, y) = do
        if y > yMax -- Stop if flowed off the bottom
            then return r
            else do
                currentSquare <- (A.readArray :: Ix i => A.STArray s i e -> i -> ST s e) r (x, y)
                if isPorous currentSquare --If this is square is porous, wet it and continue
                    then do
                        A.writeArray r (x, y) WetSand
                        go yMax (x, y + 1)
                    else do
                        -- If this square is non-porous, look at one level up
                        (solidLeft, xLeft) <- spread r False (x, y - 1)
                        (solidRight, xRight) <- spread r True (x, y - 1)
                        case (solidLeft, solidRight) of
                            -- If solid to both sides, fill between the walls and start at the top
                            (True, True) -> do
                                fillBack Water (xLeft, xRight)
                                go yMax (x, y - 2)
                            -- If solid to the left, fill and continue down right side
                            (True, False) -> do
                                fillBack WetSand (xLeft, xRight)
                                go yMax (xRight, y - 1)
                            -- If solid to the right, fill and continue down left side
                            (False, True) -> do
                                fillBack WetSand (xLeft, xRight)
                                go yMax (xLeft, y - 1)
                            -- If not solid either side flow down left and then down right
                            (False, False) -> do
                                fillBack WetSand (xLeft, xRight)
                                _ <- go yMax (xLeft, y - 1)
                                go yMax (xRight, y - 1)
      where
        --        fillBack :: Tile -> (Int, Int) -> ST s ()
        fillBack t (x0, x1) = Mon.forM_ [(i, y - 1) | i <- [x0 .. x1]] (\i -> A.writeArray r i t)

{- | Given a position above a non-porous location, look left or right (depending on
   toRight) until find either a wall (return True and the x-value) or find a location
   that does not have a non-porous location below (return False and the x-value)
-}
spread :: Reservoir s -> Bool -> (Int, Int) -> ST s (Bool, Int)
spread r toRight (xS, yS) = do
    next <- A.readArray r nextLoc
    if next == Clay
        then return (True, xS)
        else do
            nextBelow <- A.readArray r nextBelowLoc
            if not (isPorous nextBelow)
                then spread r toRight nextLoc
                else return (False, fst nextLoc)
  where
    xInc = if toRight then 1 else -1
    nextLoc = (xS + xInc, yS)
    nextBelowLoc = (xS + xInc, yS + 1)
