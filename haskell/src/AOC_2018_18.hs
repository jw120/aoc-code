{- |
 Module      : AOC_2018_18
 Description : Advent of code 2018 day 18
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_18 (solvers) where

import Data.Array.IArray (Array)
import Data.Array.IArray qualified as A (array, assocs, bounds, elems, (!))
import Data.Map (Map)
import Data.Map qualified as Map (empty, insert, lookup)
import Data.Text (Text)
import Data.Text qualified as T (length, lines, pack, unpack)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show . resources $ areaList !! 10
    , T.pack . show $ lookupLoop loop (1000000000 - skip)
    )
  where
    initialArea = readArea t
    areaList = iterate update initialArea
    -- Rather than look for a loop in the areas, instead look at resources
    -- But need to skip initial values to allow sequence to converge
    skip = 450 -- obtained manually by looking at the generated sequence
    resourceList = map resources areaList
    loop = findLoop $ drop skip resourceList

--
-- Site holds on site which can be open ground, trees or a lumber yard
---

data Site
    = OpenGround
    | Trees
    | LumberYard
    deriving (Eq)

readSite :: Char -> Site
readSite '.' = OpenGround
readSite '|' = Trees
readSite '#' = LumberYard
readSite c = error $ "Unexpected character (" ++ [c] ++ ")"

instance Show Site where
    show OpenGround = "."
    show Trees = "|"
    show LumberYard = "#"

--
-- Area is the square grid of sites indexed from top-left (0,0) to (xMax, yMax) bottom-right
--

newtype Area = Area (Array (Int, Int) Site) deriving (Eq)

instance Show Area where
    show (Area a) = init $ unlines [row y | y <- [yMin .. yMax]]
      where
        ((xMin, yMin), (xMax, yMax)) = A.bounds a
        row :: Int -> String
        row j = concat [show (a A.! (i, j)) | i <- [xMin .. xMax]]

-- | Read an Area from a string
readArea :: Text -> Area
readArea s
    | all hasValidLength stringRows = Area $ A.array ((0, 0), (size - 1, size - 1)) siteData
    | otherwise = error "Invalid size for Area"
  where
    stringRows :: [Text] = T.lines s
    size = length stringRows
    hasValidLength :: Text -> Bool
    hasValidLength = (== size) . T.length
    toData :: Int -> Text -> [((Int, Int), Site)]
    toData y rowStr = zip [(x, y) | x <- [0 .. size -1]] $ map readSite $ T.unpack rowStr
    siteData :: [((Int, Int), Site)]
    siteData = concat $ zipWith toData [0 .. size - 1] stringRows

-- | Return list with the contents of the sites adjacent to the site whose coordinates are given
adjacentSites :: (Int, Int) -> Area -> [Site]
adjacentSites (x, y) (Area a) = [a A.! (i, j) | j <- [y - 1 .. y + 1], i <- [x - 1 .. x + 1], valid (i, j)]
  where
    ((xMin, yMin), (xMax, yMax)) = A.bounds a
    valid (c, r) = (c /= x || r /= y) && c >= xMin && c <= xMax && r >= yMin && r <= yMax

--
-- Update logic
--

-- | Apply update rules to the area
update :: Area -> Area
update (Area a) = Area $ A.array (A.bounds a) (map updateSite (A.assocs a))
  where
    updateSite :: ((Int, Int), Site) -> ((Int, Int), Site)
    updateSite (coords, s) = (coords, newSite (adjacentSites coords (Area a)) s)
    newSite :: [Site] -> Site -> Site
    newSite surrounds OpenGround
        | length (filter (== Trees) surrounds) >= 3 = Trees
        | otherwise = OpenGround
    newSite surrounds Trees
        | length (filter (== LumberYard) surrounds) >= 3 = LumberYard
        | otherwise = Trees
    newSite surrounds LumberYard
        | LumberYard `notElem` surrounds = OpenGround
        | Trees `notElem` surrounds = OpenGround
        | otherwise = LumberYard

-- | Resource values of an Area
resources :: Area -> Int
resources (Area a) = trees * lumberYards
  where
    trees = length . filter (== Trees) $ A.elems a
    lumberYards = length . filter (== LumberYard) $ A.elems a

{- | Find a repeating pattern in a sequence
 Given a sequence x_0, x_1... return the repeating part and the index that the loop starts
-}
findLoop :: Ord a => [a] -> ([a], Int)
findLoop zs = (subsequence, first)
  where
    subsequence = take (firstRep - first) $ drop first zs
    (first, firstRep) = go Map.empty 0 zs
    go :: Ord b => Map b Int -> Int -> [b] -> (Int, Int)
    go m i (x : xs) = case Map.lookup x m of
        Just n -> (n, i)
        Nothing -> go (Map.insert x i m) (i + 1) xs
    go _ _ [] = error "Unexpected empty list in findLoop"

-- | Lookup a value in a repeating sequence
lookupLoop :: ([x], Int) -> Int -> x
lookupLoop (xs, start) n = xs !! nAroundLoop
  where
    nFromLoopStart = n - start
    nAroundLoop = nFromLoopStart `mod` length xs
