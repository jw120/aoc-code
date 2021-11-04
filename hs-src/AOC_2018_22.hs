{- |
 Module      : AOC_2018_22
 Description : Advent of code 2018 day 22
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_22 (solvers) where

import Data.Array (Array)
import Data.Array qualified as A (array, elems, (!))
import Data.Ix qualified as Ix (range)

import Data.Text (Text)
import Data.Text qualified as T (breakOn, drop, lines, pack, stripPrefix)
import Utilities (readUnsignedDecimal)

solvers :: Text -> (Text, Text)
solvers t = case T.lines t of
    [line0, line1] -> case (T.stripPrefix "depth: " line0, T.stripPrefix "target: " line1) of
        (Just depthText, Just targetText) ->
            let (targetXText, targetYText) = T.breakOn "," targetText
                depth = readUnsignedDecimal depthText
                targetX = readUnsignedDecimal targetXText
                targetY = readUnsignedDecimal $ T.drop 1 targetYText
             in ( T.pack . show $ riskLevel depth (targetX, targetY)
                , "NYI"
                )
        _ -> error "Malformed input"
    _ -> error "Expected two lines for input"

-- | Convert erosion Level to region Type
regionType :: Int -> Int
regionType = (`mod` 3)

-- | Convert geologic index to erosion level
erosionLevel :: Int -> Int -> Int
erosionLevel depth i = (i + depth) `mod` 20183

{- | Geologic indices of regions at depth from (0,0 ) to target (x, y)

 >>> map (geologicIndices 510 (10, 10) !) [(0, 0), (1, 0), (0, 1), (1, 1), (10, 10)]
 [0,16807,48271,145722555,0]
-}
geologicIndices :: Int -> (Int, Int) -> Array (Int, Int) Int
geologicIndices depth (xMax, yMax) = arr
  where
    arr :: Array (Int, Int) Int
    arr = A.array xyRange [(c, g c) | c <- Ix.range xyRange]
    xyRange = ((0, 0), (xMax, yMax))
    g :: (Int, Int) -> Int
    g (0, 0) = 0
    g (x, 0) = x * 16807
    g (0, y) = y * 48271
    g (x, y)
        | x == xMax && y == yMax = 0
        | otherwise = erosionLevel depth (arr A.! (x - 1, y)) * erosionLevel depth (arr A.! (x, y - 1))

{- | Risk level of area with given depth from (0, 0) to target (x, y)

 >>> riskLevel 510 (10, 10)
 114
-}
riskLevel :: Int -> (Int, Int) -> Int
riskLevel depth (xMax, yMax) = sum regionTypes
  where
    regionTypes = map (regionType . erosionLevel depth) . A.elems $ geologicIndices depth (xMax, yMax)
