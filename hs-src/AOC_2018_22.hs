{- |
 Module      : AOC_2018_22
 Description : Advent of code 2018 day 22
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_22 (solvers, riskLevel) where

import Control.Monad.State.Strict (State)
import Control.Monad.State.Strict qualified as State (evalState, get, modify)

-- import Data.Array (Array)
-- import Data.Array qualified as A (array, (!))
-- import Data.Bifunctor qualified as Bifunctor (first)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map (empty, insert, lookup)

--import Debug.Trace (trace)

import Data.Text (Text)
import Data.Text qualified as T (breakOn, drop, lines, pack, stripPrefix)

-- import Search (bfsVariable)

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

-- data RegionType = Rocky | Wet | Narrow
type Position = (Int, Int)

-- regionType :: Int -> RegionType
-- regionType erosionValue = case erosionValue `mod` 3 of
--     0 -> Rocky
--     1 -> Wet
--     _ -> Narrow

erosion :: Int -> Position -> Position -> State (Map Position Int) Int
erosion depth target = erosion'
  where
    erosion' :: Position -> State (Map Position Int) Int
    erosion' pos = (\g -> (g + depth) `mod` 20183) <$> geologic pos
    geologic :: Position -> State (Map Position Int) Int
    geologic pos = do
        m <- State.get
        case Map.lookup pos m of
            Just gExisting -> return gExisting
            Nothing -> do
                gNew <- geologic' pos
                State.modify $ Map.insert pos gNew
                return gNew
    geologic' :: Position -> State (Map Position Int) Int
    geologic' pos =
        case pos of
            (0, 0) -> return 0
            (x, 0) -> return $ x * 16807
            (0, y) -> return $ y * 48271
            p | p == target -> return 0
            (x, y) -> do
                erosionX <- erosion' (x - 1, y)
                erosionY <- erosion' (x, y - 1)
                return $ erosionX * erosionY

-- -- | Region of the position. Depth and target determine the geology.
-- region :: Int -> Position -> Position -> RegionType
-- region depth (xTarget, yTarget) pos = case erosion pos `mod` 3 of
--     0 -> Rocky
--     1 -> Wet
--     _ -> Narrow
--   where
--     erosion :: Position -> Int
--     erosion = (memoArray A.!)
--       where
--         safetyMargin :: Int = 5 -- build array beyond target
--         maxCoord = max xTarget yTarget * safetyMargin
--         memoArray :: Array Position Int
--         memoArray =
--             A.array
--                 ((0, 0), (maxCoord, maxCoord))
--                 [((x, y), (geologic (x, y) + depth) `mod` 20183) | x <- [0 .. maxCoord], y <- [0 .. maxCoord]]
--         geologic :: Position -> Int
--         geologic (0, 0) = 0
--         geologic (x, 0) = x * 16807
--         geologic (0, y) = y * 48271
--         geologic (x, y) =
--             if (x, y) == (xTarget, yTarget)
--                 then 0
--                 else erosion (x - 1, y) * erosion (x, y - 1)

-- -- | Region of the position. Depth and target determine the geology.
-- region :: Int -> Position -> Position -> RegionType
-- region depth (xTarget, yTarget) pos = case erosion pos `mod` 3 of
--     0 -> Rocky
--     1 -> Wet
--     _ -> Narrow
--   where
--     erosion :: Position -> Int
--     erosion = (memoArray A.!)
--       where
--         safetyMargin :: Int = 5 -- build array beyond target
--         maxCoord = max xTarget yTarget * safetyMargin
--         memoArray :: Array Position Int
--         memoArray =
--             A.array
--                 ((0, 0), (maxCoord, maxCoord))
--                 [((x, y), (geologic (x, y) + depth) `mod` 20183) | x <- [0 .. maxCoord], y <- [0 .. maxCoord]]
--         geologic :: Position -> Int
--         geologic (0, 0) = 0
--         geologic (x, 0) = x * 16807
--         geologic (0, y) = y * 48271
--         geologic (x, y) =
--             if (x, y) == (xTarget, yTarget)
--                 then 0
--                 else erosion (x - 1, y) * erosion (x, y - 1)

-- | Part (a): Sum of risk levels from (0, 0) to target (x, y)
riskLevel :: Int -> (Int, Int) -> Int
riskLevel depth (xTarget, yTarget) = sum $ State.evalState (mapM risk sites) Map.empty
  where
    risk :: Position -> State (Map Position Int) Int
    risk pos = (`mod` 3) <$> erosion depth (xTarget, yTarget) pos
    sites :: [Position]
    sites = [(x, y) | x <- [0 .. xTarget], y <- [0 .. yTarget]]

-- riskLevel depth (xTarget, yTarget) = sum [risk (x, y) | x <- [0 .. xTarget], y <- [0 .. yTarget]]
--   where
--     risk :: Position -> Int
--     risk = regionRisk . region depth (xTarget, yTarget)
--     regionRisk :: RegionType -> Int
--     regionRisk Rocky = 0
--     regionRisk Wet = 1
--     regionRisk Narrow = 2

-- data Tool = Climbing | Torch | Neither deriving (Eq, Ord, Show)

-- data State = State Position Tool deriving (Eq, Ord, Show)

-- Return up/down/left/right positions
-- adjacents :: Position -> [Position]
-- adjacents (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

-- Which tool is invalid at the given position
-- invalidTool :: Int -> Position -> Position -> Tool
-- invalidTool depth target pos = case region depth target pos of
--     Rocky -> Neither
--     Wet -> Torch
--     Narrow -> Climbing

-- -- Distance from origin to target location using tool switching
-- distance :: Int -> Position -> Int
-- distance depth target = case bfsVariable moves 7 targetState startState of
--     Just (dist, _path) -> dist
--     Nothing -> error "No path found"
--   where
--     startState = State (0, 0) Torch
--     targetState = State target Torch
--     moves :: Int -> State -> [State]
--     moves 1 (State pos tool) =
--         map (`State` tool)
--             . filter ((/= tool) . invalidTool depth target)
--             . filter (\(x, y) -> x >= 0 && y >= 0)
--             $ adjacents pos
--     moves 7 (State pos tool) =
--         map (State pos)
--             . filter (/= invalidTool depth target pos)
--             $ filter (/= tool) [Climbing, Torch, Neither]
--     moves _ _ = []