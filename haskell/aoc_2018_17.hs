{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AOC_2018_17 where

import Control.Monad (forM_, unless, when)
import qualified Data.Array.IO as A
import Data.Char (isDigit)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

data Tile = DrySand | WetSand | Water | Clay deriving (Eq, Show)

showTile :: Tile -> Char
showTile DrySand = ' '
showTile WetSand = '|'
showTile Water = '~'
showTile Clay = '#'

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

yStart :: Int
yStart = 1

type Reservoir = A.IOArray (Int, Int) Tile

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

parseReservoir :: [InputRecord] -> IO Reservoir
parseReservoir recs = do
  r <- A.newListArray ((xMin - 1, yMin), (xMax + 1, yMax)) $ replicate n DrySand
  forM_ recs (addRecord r)
  return r
  where
    (xMin, xMax) = foldl' joinRanges (xStart, xStart) $ map xRange recs
    (yMin, yMax) = foldl' joinRanges (yStart, yStart) $ map yRange recs
    n = (xMax - xMin + 3) * (yMax - yMin + 1)
    joinRanges :: (Int, Int) -> (Int, Int) -> (Int, Int)
    joinRanges (m0, m1) (n0, n1) = (min m0 n0, max m1 n1)
    addRecord :: Reservoir -> InputRecord -> IO ()
    addRecord r (Horizontal (x0, x1) y) = forM_ [(x, y) | x <- [x0 .. x1]] (\i -> A.writeArray r i Clay)
    addRecord r (Vertical x (y0, y1)) = forM_ [(x, y) | y <- [y0 .. y1]] (\i -> A.writeArray r i Clay)

printReservoir :: Reservoir -> IO ()
printReservoir r = do
  ((xMin, yMin), (xMax, yMax)) <- A.getBounds r
  forM_ [yMin .. yMax] $ printLine [xMin .. xMax]
  where
    printLine :: [Int] -> Int -> IO ()
    printLine xs y = do
      row <- mapM (A.readArray r) [(x, y) | x <- xs]
      putStrLn $ map showTile row

countWet :: Reservoir -> IO Int
countWet r = length . filter isWet <$> A.getElems r

run :: Bool -> Reservoir -> (Int, Int) -> IO Reservoir
run logging r loc = do
  ((_xMin, _yMin), (_xMax, yMax)) <- A.getBounds r
  go yMax loc
  where
    go :: Int -> (Int, Int) -> IO Reservoir
    go yMax (x, y) = do
      when logging $ print ("go " ++ show x ++ ", " ++ show y)
      if y > yMax -- Stop if flowed off the bottom
        then return r
        else do
          currentSquare <- A.readArray r (x, y)
          if isPorous currentSquare --If this is square is porous, wet it and continue
            then do
              when logging $ print $ "Wetting " ++ show x ++ ", " ++ show y
              A.writeArray r (x, y) WetSand
              go yMax (x, y + 1)
            else do
              -- If this square is non-porous, look at one level up
              (solidLeft, xLeft) <- spread r False (x, y - 1)
              (solidRight, xRight) <- spread r True (x, y - 1)
              case (solidLeft, solidRight) of
                -- If solid to both sides, fill between the walls and start at the top
                (True, True) -> do
                  when logging $ print $ "Backfilling " ++ show x ++ ", " ++ show (y - 1)
                  fillBack Water (xLeft, xRight)
                  back <- A.readArray r (x, y - 2)
                  unless (isPorous back) $ print ("Back track failed " ++ show x ++ ", " ++ show (y - 1) ++ " " ++ show back)
                  go yMax (x, y - 2)
                -- If solid to the left, fill and continue down right side
                (True, False) -> do
                  when logging $ print $ "Backfilling right " ++ show x ++ ", " ++ show (y - 1)
                  fillBack WetSand (xLeft, xRight)
                  go yMax (xRight, y - 1)
                -- If solid to the right, fill and continue down left side
                (False, True) -> do
                  when logging $ print $ "Backfilling left " ++ show x ++ ", " ++ show (y - 1)
                  fillBack WetSand (xLeft, xRight)
                  go yMax (xLeft, y - 1)
                -- If not solid either side flow down left and then down right
                (False, False) -> do
                  when logging $ print $ "Backfilling both " ++ show x ++ ", " ++ show (y - 1)
                  fillBack WetSand (xLeft, xRight)
                  _ <- go yMax (xLeft, y - 1)
                  go yMax (xRight, y - 1)
      where
        fillBack :: Tile -> (Int, Int) -> IO ()
        fillBack t (x0, x1) = forM_ [(i, y - 1) | i <- [x0 .. x1]] (\i -> A.writeArray r i t)

-- | Given a position above a non-porous location, look left or right (depending on
--   toRight) until find either a wall (return True and the x-value) or find a location
--   that does not have a non-porous location below (return False and the x-value)
spread :: Reservoir -> Bool -> (Int, Int) -> IO (Bool, Int)
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
  -- inputRecords <- map parseInputRecord . T.lines <$> TIO.getContents
  let inputRecords = map parseInputRecord test1
  start <- parseReservoir inputRecords
  --  printReservoir start
  final <- run True start (xStart, yStart)
  -- putStrLn "\n\n\n"
  -- printReservoir final

  n <- countWet final
  print n
