{- |
 Module      : AOC_2018_23
 Description : Advent of code 2018 day 23
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_23 (solvers, inMaxRange, Bot, pBot) where

import Data.Foldable qualified as Fold (maximumBy)
import Data.Function ((&))
import Data.Ord qualified as Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T (lines, pack)
import Debug.Trace (trace)
import Text.Megaparsec.Char qualified as MC (char, string)

import Utilities (Parser, pSignedInt, pUnsignedInt, parseOrStop)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show . length $ filter (<= botRadius strongestBot) distances
    , T.pack . show . minimum . map (distance origin . closestOrigin) . snd $ inMaxRange bots
    )
  where
    bots = map (parseOrStop pBot) $ T.lines t
    strongestBot :: Bot = Fold.maximumBy (Ord.comparing botRadius) bots
    distances :: [Int] = map (distance (botPoint strongestBot) . botPoint) bots
    origin = Point{pointX = 0, pointY = 0, pointZ = 0}

-- | Return octrees that are in range of the maximum number of bots
inMaxRange :: [Bot] -> (Int, [Oct])
inMaxRange bs = go (0, []) (fullRegion bs) bs
  where
    go :: (Int, [Oct]) -> Oct -> [Bot] -> (Int, [Oct])
    go (n, acc) _ [] = (n, acc)
    go (n, acc) oct bots
        | isUnitSize oct = trace (show (n, acc, oct, length bots)) $ goUnit (n, acc) oct bots
        | otherwise = trace (show (n, acc, oct, length bots)) $ goNonUnit (n, acc) oct bots
    goUnit :: (Int, [Oct]) -> Oct -> [Bot] -> (Int, [Oct])
    goUnit (n, acc) oct bots
        | nUnit > n = (nUnit, [oct])
        | nUnit == n = (nUnit, oct : acc)
        | otherwise = (n, acc)
      where
        nUnit = length $ filter (coversAll oct) bots
    goNonUnit (n, acc) oct bots
        | length (filter (overlaps oct) bots) < n = (n, acc)
        | otherwise =
            subtrees oct
                &
                & filterViableTrees
                & map (\(o, full, partial) -> go (n - length full, []) o (full ++ partial)
                & combineTrees
      where
        subtreesWithOverlaps = map (addOverlaps bots) $ subtrees oct
        viableTrees = filterViableTrees subtreesWithOverlaps
        exploredTrees = map ((\(o, full, partial) -> go (threshold, []) o (full ++ partial)) viableTrees

        addOverlaps :: [Bot] -> Oct -> (Oct, [Bot], [Bot])
        addOverlaps bb o = (o, filter (coversAll o) bb, filter (\b -> overlaps o b && not (coversAll o b)) bb)
        filterViableTrees :: [(Oct, [Bot], [Bot])] -> [(Oct, [Bot], [Bot])]
        filterViableTrees xs =
            map (\(o, full, partial, _minN, _maxN) -> (o, full, partial)) $
                filter (\(_o, _full, _partial, _minN, maxN) -> maxN >= threshold) os'
          where
            os' = map (\(o, full, partial) -> (o, full, partial, length full, length full + length partial)) xs
            highestMin = maximum $ map (\(_o, _full, _partial, minN, _maxN) -> minN) os'
            threshold = max n highestMin

        combineTrees :: [(Int, [Oct])] -> (Int, [Oct])
        combineTrees results = (nMax, concatMap snd $ filter ((== nMax) . fst) results)
          where
            nMax = maximum $ map fst results

--
-- Points
--

data Point = Point
    { pointX :: Int
    , pointY :: Int
    , pointZ :: Int
    }

instance Show Point where
    show p = "<" ++ show (pointX p) ++ "," ++ show (pointY p) ++ "," ++ show (pointZ p) ++ ">"

-- Manhattan distance between two points
distance :: Point -> Point -> Int
distance Point{pointX = x1, pointY = y1, pointZ = z1} Point{pointX = x2, pointY = y2, pointZ = z2} =
    abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

--
-- Bot support
--

data Bot = Bot
    { botPoint :: Point
    , botRadius :: Int
    }

instance Show Bot where
    show b = "pos=" ++ show (botPoint b) ++ ", r=" ++ show (botRadius b)

pBot :: Parser Bot
pBot = do
    x <- MC.string "pos=<" *> pSignedInt
    y <- MC.char ',' *> pSignedInt
    z <- MC.char ',' *> pSignedInt
    r <- MC.string ">, r=" *> pUnsignedInt
    return Bot{botPoint = Point{pointX = x, pointY = y, pointZ = z}, botRadius = r}

-- Does the bot cover the given point?
covers :: Point -> Bot -> Bool
covers z Bot{botPoint = p, botRadius = r} = distance p z <= r

--
-- Octree support
--

-- Octree region of space bounded by x0 <= x <= x1 etc
data Oct = Oct
    { octX :: (Int, Int)
    , octY :: (Int, Int)
    , octZ :: (Int, Int)
    }
    deriving (Eq, Show)

-- | Return the distinct corners of an octree region
corners :: Oct -> [Point]
corners oct =
    [ Point{pointX = x, pointY = y, pointZ = z}
    | x <- pairToList $ octX oct
    , y <- pairToList $ octY oct
    , z <- pairToList $ octZ oct
    ]
  where
    pairToList (a, b)
        | a == b = [a]
        | otherwise = [a, b]

-- | Is the octree of unit size
isUnitSize :: Oct -> Bool
isUnitSize oct = isUnit (octX oct) && isUnit (octY oct) && isUnit (octZ oct)
  where
    isUnit (a, b) = a == b

-- | Closest Point to the origin within the octree
closestOrigin :: Oct -> Point
closestOrigin Oct{octX = x, octY = y, octZ = z} = Point{pointX = closest x, pointY = closest y, pointZ = closest z}
  where
    closest :: (Int, Int) -> Int
    closest (a, b)
        | a <= 0 && b >= 0 = 0
        | a >= 0 = a
        | otherwise = b

-- | Return up to 8 octrees that are the children of this octree
subtrees :: Oct -> [Oct]
subtrees oct =
    [ Oct{octX = x, octY = y, octZ = z}
    | x <- splitRange (octX oct)
    , y <- splitRange (octY oct)
    , z <- splitRange (octZ oct)
    ]
  where
    splitRange :: (Int, Int) -> [(Int, Int)]
    splitRange (a, b)
        | a == b = [(a, a)]
        | otherwise = [(a, mid), (mid + 1, b)]
      where
        mid = (a + b) `div` 2

--
-- Combining Octree and bot
--

-- Does the bot cover all of the octree?
coversAll :: Oct -> Bot -> Bool
coversAll oct bot = all (`covers` bot) $ corners oct

-- Does the bot cover any part of the octree?
overlaps :: Oct -> Bot -> Bool
overlaps
    Oct{octX = ox, octY = oy, octZ = oz}
    Bot{botPoint = Point{pointX = px, pointY = py, pointZ = pz}, botRadius = r} =
        dist ox px + dist oy py + dist oz pz <= r
      where
        dist (a, b) c
            | a <= c && c <= b = 0
            | otherwise = min (abs (a - c)) (abs (b - c))

-- | Create the smallest octree that includes the positions of all the bots given
fullRegion :: [Bot] -> Oct
fullRegion bots =
    Oct
        { octX = (minimum xs, maximum xs)
        , octY = (minimum ys, maximum ys)
        , octZ = (minimum zs, maximum zs)
        }
  where
    xs = map (pointX . botPoint) bots
    ys = map (pointY . botPoint) bots
    zs = map (pointZ . botPoint) bots
