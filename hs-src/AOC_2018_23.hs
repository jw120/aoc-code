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

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Int
distance (a, b, c) (d, e, f) = abs (a - d) + abs (b - e) + abs (c - f)

pBot :: Parser Bot
pBot = do
    x <- MC.string "pos=<" *> pSignedInt
    y <- MC.char ',' *> pSignedInt
    z <- MC.char ',' *> pSignedInt
    r <- MC.string ">, r=" *> pUnsignedInt
    return Bot{botX = x, botY = y, botZ = z, botR = r}

-- | Solve part A, number of bots in range of the strongest bot
partA :: [Bot] -> Int
partA bots = length $ filter (<= botR strongestBot) distances
  where
    strongestBot :: Bot = Fold.maximumBy (Ord.comparing botR) bots
    distances :: [Int] = map (botDistance strongestBot) bots

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

-- corners :: Oct -> [(Int, Int, Int)]
-- corners o = [(x, y, z) | x <- [x0 o, x1 o], y <- [y0 o, y1 o], z <- [z0 o, z1 o]]

-- unitSize :: Oct -> Bool
-- unitSize o = x0 o == x1 o && y0 o == y1 o && z0 o == z1 o

{-

-- | Solve part B
partB :: [Bot] -> Int
partB = closestOrigin . mostInRange

-- | Closest distance to origin within the octree
--
-- >>> closestOrigin $ fullRegion testA
-- 0
-- >>> closestOrigin $ Oct {x0 = 1, x1 = 2, y0 = 2, y1 = 3, z0 = 3, z1 = 9}
-- 6
closestOrigin :: Oct -> Int
closestOrigin o = closest (x0 o) (x1 o) + closest (y0 o) (y1 o) + closest (z0 o) (z1 o)
  where
    closest :: Int -> Int -> Int
    closest a b = min (abs a) (abs b)

-- | Octree that spans the points given
--
-- >>> fullRegion testA
-- Oct {x0 = 0, x1 = 4, y0 = 0, y1 = 5, z0 = 0, z1 = 3}
fullRegion :: [Bot] -> Oct
fullRegion bots =
  Oct
    { x0 = minimum xs,
      x1 = maximum xs,
      y0 = minimum ys,
      y1 = maximum ys,
      z0 = minimum zs,
      z1 = maximum zs
    }
  where
    xs = map x bots
    ys = map y bots
    zs = map z bots

-- | Return octree that is in range of the most points
mostInRange :: [Bot] -> Int
mostInRange bots = go $ fullRegion bots
  where
    go :: Oct -> Int
    go oct
      | unitSize oct = length fullyInside + length overlapping
      | otherwise = undefined
      where
        (fullyInside, fullyOutside, overlapping) = splitOnOct oct bots

-- | Divide
splitOnOct :: Oct -> [Bot] -> ([Bot], [Bot], [Bot])
splitOnOct o bots = (filter (isFullyInside o) bots, filter (isFullyOutside o) bots, filter neither bots)
  where
    neither :: Bot -> Bool
    neither b = not (isFullyInside o b) && not (isFullyOutside o b)

-- | Is the bot centre within the Octree
isInside :: Oct -> Bot -> Bool
isInside o b =
  x0 o <= x b && x b <= x1 o
    && y0 o <= y b
    && y b <= y1 o
    && z0 o <= z b
    && z b <= z1 o

-- | Is the full range of the bot contained within the Octree
isFullyInside :: Oct -> Bot -> Bool
isFullyInside o b =
  x0 o <= x b - r b && x b + r b <= x1 o
    && y0 o <= y b - r b
    && y b + r b <= y1 o
    && z0 o <= z b - r b
    && z b + r b <= z1 o

-- | Is no part of the range of the bot overlapping the Octree
isFullyOutside :: Oct -> Bot -> Bool
isFullyOutside o b = not (isInside o b) && farAway
  where
    farAway = all ((> r b) . distance (x b, y b, z b)) $ corners o

-}

-- testA :: [Bot]
-- testA =
--     map
--         readBot
--         [ "pos=<0,0,0>, r=4"
--         , "pos=<1,0,0>, r=1"
--         , "pos=<4,0,0>, r=3"
--         , "pos=<0,2,0>, r=1"
--         , "pos=<0,5,0>, r=3"
--         , "pos=<0,0,3>, r=1"
--         , "pos=<1,1,1>, r=1"
--         , "pos=<1,1,2>, r=1"
--         , "pos=<1,3,1>, r=1"
--         ]
