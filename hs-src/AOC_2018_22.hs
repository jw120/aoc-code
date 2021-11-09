{- |
 Module      : AOC_2018_22
 Description : Advent of code 2018 day 22
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_22 (solvers, riskLevel) where

import Data.Array (Array)
import Data.Array qualified as A (array, elems, (!))
import Data.Ix qualified as Ix (range)
import Data.Text (Text)
import Data.Text qualified as T (breakOn, drop, lines, pack, stripPrefix)

import Search (bfsVariable)
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
                , T.pack . show $ distance depth (targetX, targetY)
                )
        _ -> error "Malformed input"
    _ -> error "Expected two lines for input"

data RegionType = Rocky | Wet | Narrow
newtype ErosionLevel = ErosionLevel Int
newtype GeologicalIndex = GeologicalIndex Int
type Position = (Int, Int)
data Tool = Climbing | Torch | Neither deriving (Eq, Show)
data State = State {statePos :: Position, stateTool :: Tool}

-- | Convert erosion Level to region Type
regionType :: ErosionLevel -> RegionType
regionType (ErosionLevel i) = case i `mod` 3 of
    0 -> Rocky
    1 -> Wet
    _ -> Narrow

-- | Convert geological index to erosion level
erosionLevel :: Int -> GeologicalIndex -> ErosionLevel
erosionLevel depth (GeologicalIndex g) = ErosionLevel $ (depth + g) `mod` 20183

-- | Geologic indices of regions at depth from (0,0 ) to target (x, y)
geologicIndex :: Int -> Position -> Position -> GeologicIndex
geologicIndex depth target pos = (geologicIndices depth target) A.! pos

-- | Geologic indices of regions at depth from (0,0 ) to target (x, y)
geologicIndices :: Int -> (Int, Int) -> Array (Int, Int) Int
geologicIndices depth (xMax, yMax) = A.array xyRange [(c, g c) | c <- Ix.range xyRange]
  where
    xyRange = ((0, 0), (2 * xMax, 2 * yMax))
    g :: (Int, Int) -> Int
    g (0, 0) = 0
    g (x, 0) = x * 16807
    g (0, y) = y * 48271
    g (x, y)
        | x == xMax && y == yMax = 0
        | otherwise = erosionLevel depth (arr A.! (x - 1, y)) * erosionLevel depth (arr A.! (x, y - 1))

-- | Part (a): Sum of risk levels from (0, 0) to target (x, y)
riskLevel :: Int -> (Int, Int) -> Int
riskLevel depth (xMax, yMax) = sum regionTypes
  where
    regionTypes = map (regionRisk . regionType . erosionLevel depth) . A.elems $ geologicIndices depth (xMax, yMax)
    regionRisk :: RegionType -> Int
    regionRisk Rocky = RiskLevel 0
    regionRisk Wet = 1
    regionRisk Narrow = 2

-- Return up/down/left/right positions
adjacents :: Position -> [Position]
adjacents (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

-- Which tool is invalid at the given position
invalidTool :: Int -> Position -> Tool
invalidTool = case regionType (erosionLevel depth pos) of
    Rocky -> Neither
    Wet -> Torch
    Narrow -> Climbing

-- Distance from origin to given location using tool switching
distance :: Int -> (Int, Int) -> Int
distance depth target = fst . bfsVariable moves 7 targetState startState
  where
    stateState = State{statePos = (0, 0), stateTool = Torch}
    targetState = State{statePos = target, stateTool = Torch}
    moves :: Int -> State -> [State]
    moves 1 State{statePos = pos, stateTool = tool} =
        filter ((/= tool) . invalidTool depth) $ adjacents pos
    moves 7 State{statePos = pos, stateTool = tool} =
        filter (/= invalidTool depth pos) $ filter (/= tool) [Climbing, Torch, Neither]
    moves _ = []