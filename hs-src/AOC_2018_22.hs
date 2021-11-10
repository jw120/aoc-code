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
import Data.Array qualified as A (array, (!))
import Debug.Trace (trace)

-- import Data.Ix qualified as Ix (range)
import Data.Text (Text)
import Data.Text qualified as T (breakOn, drop, lines, pack, stripPrefix)

-- import Search (bfsVariable)

import Data.Map.Strict (mapKeysMonotonic)
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
                , "NYI" -- T.pack . show $ distance depth (targetX, targetY)
                )
        _ -> error "Malformed input"
    _ -> error "Expected two lines for input"

data RegionType = Rocky | Wet | Narrow

-- newtype ErosionLevel = ErosionLevel Int
-- newtype GeologicIndex = GeologicIndex Int
type Position = (Int, Int)
data Tool = Climbing | Torch | Neither deriving (Eq, Show)

-- data State = State {statePos :: Position, stateTool :: Tool}

-- -- | Convert erosion Level to region Type
-- regionType :: ErosionLevel -> RegionType
-- regionType (ErosionLevel i) = case i `mod` 3 of
--     0 -> Rocky
--     1 -> Wet
--     _ -> Narrow

{- | Convert geological index to erosion level
 erosionLevel :: Int -> GeologicIndex -> ErosionLevel
 erosionLevel depth (GeologicIndex g) = ErosionLevel $ (depth + g) `mod` 20183
-}

-- | Region of the position. Depth and target determine the geology.
region :: Int -> Position -> Position -> RegionType
region depth (xTarget, yTarget) pos = case erosion pos `mod` 3 of
    0 -> Rocky
    1 -> Wet
    _ -> Narrow
  where
    erosion :: Position -> Int
    erosion p = memoArray A.! p
      where
        safetyMargin :: Int = 2 -- build array beyond target
        xMax = xTarget * safetyMargin
        yMax = yTarget * safetyMargin
        memoArray :: Array Position Int
        memoArray =
            A.array
                ((0, 0), (xMax, yMax))
                [((x, y), (geologic (x, y) + depth) `mod` 20183) | x <- [0 .. xMax], y <- [0 .. yMax]]
        geologic :: Position -> Int
        geologic (0, 0) = 0
        geologic (x, 0) = x * 16807
        geologic (0, y) = y * 48271
        geologic (x, y) =
            trace (show (x, y)) $
                if (x, y) == (xTarget, yTarget)
                    then 0
                    else erosion (x - 1, y) * erosion (x, y - 1)

{- | Geologic indices of regions at depth from (0,0 ) to target (x, y)
 geologicIndex :: Int -> Position -> Position -> GeologicIndex
 geologicIndex depth target pos = GeologicIndex $ geologicIndices depth target A.! pos
-}

{- | Geologic indices of regions at depth from (0,0 ) to target (x, y)
 Memoizied using a lazy map.
-}

-- geologicIndices :: Int -> (Int, Int) -> Array (Int, Int) Int
-- geologicIndices depth pos = A.array xyRange [(c, g c) | c <- Ix.range xyRange]
--   where
--     xyRange = ((0, 0), (2 * xMax, 2 * yMax))
--     g :: (Int, Int) -> Int
--     g (0, 0) = 0
--     g (x, 0) = x * 16807
--     g (0, y) = y * 48271
--     g (x, y)
--         | x == xMax && y == yMax = 0
--         | otherwise = erosionLevel depth (arr A.! (x - 1, y)) * erosionLevel depth (arr A.! (x, y - 1))

-- memoized_fib :: Int -> Integer
-- memoized_fib = (map fib [0 ..] !!)
--    where fib 0 = 0
--          fib 1 = 1
--          fib n = memoized_fib (n-2) + memoized_fib (n-1)

-- | Part (a): Sum of risk levels from (0, 0) to target (x, y)
riskLevel :: Int -> (Int, Int) -> Int
riskLevel depth (xTarget, yTarget) = sum [risk (x, y) | x <- [0 .. xTarget], y <- [0 .. yTarget]]
  where
    risk :: Position -> Int
    risk = regionRisk . region depth (xTarget, yTarget)
    regionRisk :: RegionType -> Int
    regionRisk Rocky = 0
    regionRisk Wet = 1
    regionRisk Narrow = 2

-- Return up/down/left/right positions
-- adjacents :: Position -> [Position]
-- adjacents (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

-- Which tool is invalid at the given position
-- invalidTool :: Int -> Position -> Tool
-- invalidTool = case regionType (erosionLevel depth pos) of
--     Rocky -> Neither
--     Wet -> Torch
--     Narrow -> Climbing

-- -- Distance from origin to given location using tool switching
-- distance :: Int -> (Int, Int) -> Int
-- distance depth target = fst . bfsVariable moves 7 targetState startState
--   where
--     stateState = State{statePos = (0, 0), stateTool = Torch}
--     targetState = State{statePos = target, stateTool = Torch}
--     moves :: Int -> State -> [State]
--     moves 1 State{statePos = pos, stateTool = tool} =
--         filter ((/= tool) . invalidTool depth) $ adjacents pos
--     moves 7 State{statePos = pos, stateTool = tool} =
--         filter (/= invalidTool depth pos) $ filter (/= tool) [Climbing, Torch, Neither]
--     moves _ _ = []