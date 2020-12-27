{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AOC_2018_17 where

import Control.Monad (forM_)
import Data.Array.IArray (Array, (!), (//))
import qualified Data.Array.IArray as A
import Data.Char (isDigit)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

data Tile = DrySand | WetSand | Water | Clay

showTile :: Tile -> Char
showTile DrySand = '.'
showTile WetSand = '|'
showTile Water = '~'
showTile Clay = '#'

isPorous :: Tile -> Bool
isPorous DrySand = True
isPorous WetSand = True
isPorous _ = False

isSolid :: Tile -> Bool
isSolid = not . isPorous

isWet :: Tile -> Bool
isWet WetSand = True
isWet Water = True
isWet _ = False

-- Location below the spring
xStart :: Int
xStart = 500

yStart :: Int
yStart = 1

type Reservoir = Array (Int, Int) Tile

data InputRecord = Horizontal (Int, Int) Int | Vertical Int (Int, Int) deriving (Show)

xRange :: InputRecord -> (Int, Int)
xRange (Horizontal xR _) = xR
xRange (Vertical x _) = (x, x)

yRange :: InputRecord -> (Int, Int)
yRange (Horizontal _ y) = (y, y)
yRange (Vertical _ yR) = yR

parseInputRecord :: Text -> InputRecord
parseInputRecord input
  | isVertical = Vertical (head nums) (nums !! 1, nums !! 2)
  | otherwise = Horizontal (nums !! 1, nums !! 2) (head nums)
  where
    isVertical :: Bool = T.head input == 'x'
    nums :: [Int] = map readInt . filter (not . T.null) . T.split (not . isDigit) $ T.drop 2 input
    readInt :: Text -> Int
    readInt t = i
      where
        Right (i, _) = TR.decimal t

parseReservoir :: [InputRecord] -> Reservoir
parseReservoir recs = foldl' addRecord emptyReservoir recs
  where
    (xMin, xMax) = foldl' joinRanges (xStart, xStart) $ map xRange recs
    (yMin, yMax) = foldl' joinRanges (yStart, yStart) $ map yRange recs
    n = (xMax - xMin + 3) * (yMax - yMin + 1)
    joinRanges :: (Int, Int) -> (Int, Int) -> (Int, Int)
    joinRanges (m0, m1) (n0, n1) = (min m0 n0, max m1 n1)
    emptyReservoir = A.listArray ((xMin - 1, yMin), (xMax + 1, yMax)) $ replicate n DrySand
    addRecord :: Reservoir -> InputRecord -> Reservoir
    addRecord r (Horizontal (x0, x1) y) = r // [((x, y), Clay) | x <- [x0 .. x1]]
    addRecord r (Vertical x (y0, y1)) = r // [((x, y), Clay) | y <- [y0 .. y1]]

printReservoir :: Reservoir -> IO ()
printReservoir r = forM_ [yMin .. yMax] printLine
  where
    ((xMin, yMin), (xMax, yMax)) = A.bounds r
    printLine :: Int -> IO ()
    printLine y = putStrLn [showTile (r ! (x, y)) | x <- [xMin .. xMax]]

countWet :: Reservoir -> Int
countWet = length . filter isWet . A.elems

run :: Int -> Reservoir -> (Int, Int) -> Reservoir
run n r (x, y)
  | n == 0 = r
  -- Stop if flowed off the bottom
  | y > yMax = r
  -- If this is square is porous, wet it and continue
  | isPorous currentSquare = run (n - 1) (r // [((x, y), WetSand)]) (x, y + 1)
  -- If this square is non-porous, look at one level up
  | otherwise = case (solidLeft, solidRight) of
    -- If solid to both sides, fill between the walls and start at the top
    (True, True) -> run (n - 1) (fillBack Water) (x, y - 2)
    -- If solid to the left, fill and continue down right side
    (True, False) -> run (n - 1) (fillBack WetSand) (xRight, y - 1)
    -- If solid to the right, fill and continue down left side
    (False, True) -> run (n - 1) (fillBack WetSand) (xLeft, y - 1)
    -- If not solid either side flow down left and then down right
    (False, False) ->
      let r' = run (n - 1) (fillBack WetSand) (xLeft, y - 1)
       in run (n - 1) r' (xRight, y - 1)
  where
    ((_xMin, _yMin), (_xMax, yMax)) = A.bounds r
    currentSquare = r ! (x, y)
    (solidLeft, xLeft) = spread r False (x, y - 1)
    (solidRight, xRight) = spread r True (x, y - 1)
    fillBack :: Tile -> Reservoir
    fillBack t = r // [((i, y - 1), t) | i <- [xLeft .. xRight]]

-- | Given a position above a non-porous location, look left or right (depending on
--   toRight) until find either a wall (return True and the x-value) or find a location
--   that does not have a non-porous location below (return False and the x-value)
spread :: Reservoir -> Bool -> (Int, Int) -> (Bool, Int)
spread r toRight (xS, yS)
  | isSolid (r ! next) = (True, xS)
  | isSolid (r ! nextBelow) = spread r toRight next
  | otherwise = (False, fst next)
  where
    xInc = if toRight then 1 else -1
    next = (xS + xInc, yS)
    nextBelow = (xS + xInc, yS + 1)

test1 :: [Text]
test1 =
  [ "x=495, y=2..7",
    "y=7, x=495..501",
    "x=501, y=3..7",
    "x=498, y=2..4",
    "x=506, y=1..2",
    "x=498, y=10..13",
    "x=504, y=10..13",
    "y=13, x=498..504"
  ]

main :: IO ()
main = do
  inputRecords <- map parseInputRecord . T.lines <$> TIO.getContents
  --  let inputRecords = map parseInputRecord test1
  -- print $ take 5 inputRecords
  let start = parseReservoir inputRecords
  --  printReservoir start
  let final = run (-1) start (xStart, yStart)
  -- putStrLn "\n\n\n"
  printReservoir final

  print $ countWet final
