{- |
 Module      : AOC_2018_23
 Description : Advent of code 2018 day 23
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_23 (solvers) where

import Data.Foldable qualified as Fold (maximumBy)
import Data.Function ((&))
import Data.List qualified as L (partition)
import Data.Ord qualified as Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T (lines, pack)
import Text.Megaparsec.Char qualified as MC (char, string)

import Utilities (Parser, pSignedInt, pUnsignedInt, parseOrStop)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ partA bots
    , "NYI"
    )
  where
    bots = map (parseOrStop pBot) $ T.lines t

-- | Solve part A, number of bots in range of the strongest bot
partA :: [Bot] -> Int
partA bots = length $ filter (<= botR strongestBot) distances
  where
    strongestBot :: Bot = Fold.maximumBy (Ord.comparing botR) bots
    distances :: [Int] = map (botDistance strongestBot) bots

-- | Solve part B
partB :: [Bot] -> Int
partB = closestOrigin . mostInRange

--
-- Bot support
--

data Bot = Bot
    { botX :: Int
    , botY :: Int
    , botZ :: Int
    , botR :: Int
    }

instance Show Bot where
    show b = "pos=<" ++ show (botX b) ++ "," ++ show (botY b) ++ "," ++ show (botZ b) ++ ">, r=" ++ show (botR b)

botDistance :: Bot -> Bot -> Int
botDistance b1 b2 = distance (botX b1, botY b1, botZ b1) (botX b2, botY b2, botZ b2)

-- Manhattan distance between two points
distance :: (Int, Int, Int) -> (Int, Int, Int) -> Int
distance (a, b, c) (d, e, f) = abs (a - d) + abs (b - e) + abs (c - f)

pBot :: Parser Bot
pBot = do
    x <- MC.string "pos=<" *> pSignedInt
    y <- MC.char ',' *> pSignedInt
    z <- MC.char ',' *> pSignedInt
    r <- MC.string ">, r=" *> pUnsignedInt
    return Bot{botX = x, botY = y, botZ = z, botR = r}

-- Does the bot cover the given point
covers :: Bot -> (Int, Int, Int) -> Bool
covers Bot{botX = x, botY = y, botZ = z, botR = r} (a, b, c) = distance (x, y, z) (a, b, c) <= r

--
-- Octree support
--

-- Octree region of space bounded by x0 <= x <= x1 etc
data Oct = Oct
    { x0 :: Int
    , x1 :: Int
    , y0 :: Int
    , y1 :: Int
    , z0 :: Int
    , z1 :: Int
    }
    deriving (Show)

-- | Return the 8 corners of an octree region
corners :: Oct -> [(Int, Int, Int)]
corners o = [(x, y, z) | x <- [x0 o, x1 o], y <- [y0 o, y1 o], z <- [z0 o, z1 o]]

-- | Is the octree of unit size
isUnitSize :: Oct -> Bool
isUnitSize o = x0 o == x1 o && y0 o == y1 o && z0 o == z1 o

-- | Smallest octree that includes the positions of all the bots given
fullRegion :: [Bot] -> Oct
fullRegion bots =
    Oct
        { x0 = minimum xs
        , x1 = maximum xs
        , y0 = minimum ys
        , y1 = maximum ys
        , z0 = minimum zs
        , z1 = maximum zs
        }
  where
    xs = map botX bots
    ys = map botY bots
    zs = map botZ bots

-- | Divide a list of bots into those that cover all or part of the octree
filterCovers :: Oct -> [Bot] -> ([Bot], [Bot])
filterCovers oct bots = L.partition (coversAll oct) $ filter (overlaps oct) bots
  where
    -- does the bot cover any part of the octree?
    overlaps :: Oct -> Bot -> Bool
    overlaps (Oct x0 x1 y0 y1 z0 z1) (Bot x y z r) =
        x0 - r <= x && x <= x1 + r && y0 - r <= y && y <= y1 + r && z0 - r <= z && z <= z1 + r
    -- does the bot cover all of the octree?
    coversAll :: Oct -> Bot -> Bool
    coversAll o bot = all (covers bot) $ corners o

--
--
--

-- | Return octrees that are in range of the maximum number of bots
inMaxRange :: [Bot] -> (Int, [Oct])
inMaxRange b = go (0, []) (fullRegion b) b
  where
    go :: (Int, [Oct]) -> Oct -> [Bot] -> (Int, [Oct])
    go (n, acc) oct bots
        | isUnitSize oct = goUnit (n, acc) oct bots
        | otherwise = goNonUnit (n, acc) oct bots
    goUnit :: (Int, [Oct]) -> Oct -> [Bot] -> (Int, [Oct])
    goUnit (n, acc) oct bots
        | nUnit > n = (nUnit, [oct])
        | nUnit == n = (nUnit, oct : acc)
        | otherwise = (n, acc)
      where
        nUnit = length . fst $ filterCovers oct bots
    goNonUnit (n, acc) oct bots =
        let (fullyCovers, partiallyCovers) = filterCovers o bots
         in if length fullyCovers + length partiallyCovers < n
                then (n, acc)
                else
                    subtrees oct
                        & map (\o -> (o, filterCovers o bots))
                        & filter (\(_o, (full, partial)) -> length full + length partial >= n)
                        & map (\(o, (full, partial)) -> (length full, go (n - length full, []) o partial))
                        & combineTrees
      where
        combineTrees :: [(Int, (Int, [Oct]))] -> (Int, [Oct])
        combineTrees = undefined

{- | Closest distance to origin within the octree

 >>> closestOrigin $ fullRegion testA
 0
 >>> closestOrigin $ Oct {x0 = 1, x1 = 2, y0 = 2, y1 = 3, z0 = 3, z1 = 9}
 6
-}
closestOrigin :: Oct -> Int
closestOrigin o = closest (x0 o) (x1 o) + closest (y0 o) (y1 o) + closest (z0 o) (z1 o)
  where
    closest :: Int -> Int -> Int
    closest a b = min (abs a) (abs b)
