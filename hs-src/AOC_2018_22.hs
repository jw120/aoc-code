{- |
 Module      : AOC_2018_22
 Description : Advent of code 2018 day 22
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_22 (solvers, riskLevel, distance) where

import Control.Monad (filterM)
import Control.Monad.State.Strict (State)
import Control.Monad.State.Strict qualified as State (evalState, get, modify)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map (empty, insert, lookup)
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
type Position = (Int, Int)
type GeoMap = Map Position Int

regionType :: Int -> RegionType
regionType erosionValue = case erosionValue `mod` 3 of
    0 -> Rocky
    1 -> Wet
    _ -> Narrow

erosion :: Int -> Position -> Position -> State GeoMap Int
erosion depth target = erosion'
  where
    erosion' :: Position -> State GeoMap Int
    erosion' pos = (\g -> (g + depth) `mod` 20183) <$> geologic pos
    geologic :: Position -> State GeoMap Int
    geologic pos = do
        m <- State.get
        case Map.lookup pos m of
            Just gExisting -> return gExisting
            Nothing -> do
                gNew <- geologic' pos
                State.modify $ Map.insert pos gNew
                return gNew
    geologic' :: Position -> State GeoMap Int
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

-- | Part (a): Sum of risk levels from (0, 0) to target (x, y)
riskLevel :: Int -> (Int, Int) -> Int
riskLevel depth (xTarget, yTarget) = sum $ State.evalState (mapM risk sites) Map.empty
  where
    risk :: Position -> State GeoMap Int
    risk pos = (`mod` 3) <$> erosion depth (xTarget, yTarget) pos
    sites :: [Position]
    sites = [(x, y) | x <- [0 .. xTarget], y <- [0 .. yTarget]]

data Tool = Climbing | Torch | Neither deriving (Eq, Ord, Show)
data CaveState = CaveState Position Tool deriving (Eq, Ord, Show)

-- Return up/down/left/right positions
adjacents :: Position -> [Position]
adjacents (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

-- Is the position accessible with this tool?
isAccessible :: Int -> Position -> Tool -> Position -> State GeoMap Bool
isAccessible depth target tool pos = do
    erosionValue <- erosion depth target pos
    return $
        tool /= case regionType erosionValue of
            Rocky -> Neither
            Wet -> Torch
            Narrow -> Climbing

-- -- Distance from origin to target location using tool switching
distance :: Int -> Position -> Int
distance depth target = case State.evalState (bfsVariable moves 7 targetState startState) Map.empty of
    Just (dist, _path) -> dist
    Nothing -> error "No path found"
  where
    startState = CaveState (0, 0) Torch
    targetState = CaveState target Torch
    moves :: Int -> CaveState -> State GeoMap [CaveState]
    moves 1 (CaveState pos tool) = do
        let available = filter (\(x, y) -> x >= 0 && y >= 0) $ adjacents pos
        accessible <- filterM (isAccessible depth target tool) available
        return $ map (`CaveState` tool) accessible
    moves 7 (CaveState pos tool) = do
        let otherTools = filter (/= tool) [Climbing, Torch, Neither]
        accessibleTools <- filterM (\t -> isAccessible depth target t pos) otherTools
        return $ map (CaveState pos) accessibleTools
    moves _ _ = return []