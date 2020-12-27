{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AOC_2018_17 where

import Control.Monad (forM_)
import Data.Array.IArray (Array, (!), (//))
import qualified Data.Array.IArray as A
import Data.Char (isDigit)
import Data.Either (fromRight)
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
    (xMin, xMax) = foldl' joinRanges (500, 500) $ map xRange recs
    (yMin, yMax) = foldl' joinRanges (1, 1) $ map yRange recs
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
countWet = undefined

run :: Reservoir -> Reservoir
run = undefined

test1 :: [Text] =
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
  --  inputRecords <- map parseInputRecord . T.lines <$> TIO.getContents
  let inputRecords = map parseInputRecord test1
  print inputRecords
  let start = parseReservoir inputRecords
  print $ A.bounds start

  printReservoir start

--  let final = run start
-- print $ countWet final
