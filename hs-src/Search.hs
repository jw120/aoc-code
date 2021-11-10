{- |
 Module      : Search
 Description : Shared code for BFS
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module Search (bfsBasic, bfsVariable) where

import Data.List qualified as L (foldl')
import Data.Map (Map)
import Data.Map qualified as Map (empty, filter, fromList, keys, keysSet, lookup, map, null, singleton, toList, union)
import Data.Set (Set)
import Data.Set qualified as Set (difference, fromList, member, null, singleton, toList, union)

--import Debug.Trace (trace)

-- | Basic breadth-first search with single-distance steps and single end-state. Returns distance and path.
bfsBasic ::
    forall position.
    Ord position =>
    -- | Function to generates list of moves available from current position
    (position -> [position]) ->
    -- | Finish state
    position ->
    -- | Starting state
    position ->
    -- | Returns distance from start to finish and one of the shortest back-paths from finish to start if one exists
    Maybe (Int, [position])
bfsBasic moves finishPosition startPosition =
    case go 0 (Set.singleton startPosition) (Set.singleton startPosition) (Map.singleton startPosition Nothing) of
        Nothing -> Nothing
        Just (distance, _, _, trackBackMap) -> Just (distance, trackBack trackBackMap finishPosition)
  where
    go ::
        -- | Distance from starting position
        Int ->
        -- | All the positions which we have visited
        Set position ->
        -- | Frontier subset of visited positions most recently visited - only consider moves from here
        Set position ->
        -- Map for back-tracking, holds the incoming position for each visited position
        Map position (Maybe position) ->
        Maybe (Int, Set position, Set position, Map position (Maybe position))
    go distance visited frontier traceBack
        | finishPosition `Set.member` frontier = Just (distance, visited, frontier, traceBack)
        | Set.null frontier = Nothing
        | otherwise = go (distance + 1) visited' frontier' traceBack'
      where
        movesFromFrontier = L.foldl' addNextPositions Map.empty $ Set.toList frontier
        frontier' = Set.fromList (Map.keys movesFromFrontier) `Set.difference` visited
        visited' = visited `Set.union` Map.keysSet movesFromFrontier
        traceBack' = traceBack `Map.union` movesFromFrontier
        addNextPositions :: Map position (Maybe position) -> position -> Map position (Maybe position)
        addNextPositions m pos = Map.union m nextPositions
          where
            nextPositions = Map.fromList . map (,Just pos) $ moves pos

-- | Breadth-first search with variable-distance steps and single end-state. Returns distance and path.
bfsVariable ::
    forall position.
    Ord position =>
    -- | Function to generates list of moves with the given incremental distance available from current position
    (Int -> position -> [position]) ->
    -- Longest distance move to be consider
    Int ->
    -- | Finish state
    position ->
    -- | Starting state
    position ->
    -- | Returns distance from start to finish and one of the shortest back-paths from finish to start if one exists
    Maybe (Int, [position])
bfsVariable moves longestMove finishPosition startPosition =
    case go 0 (Map.singleton startPosition 0) (Map.singleton startPosition 0) (Map.singleton startPosition Nothing) of
        Nothing -> Nothing
        Just (distance, _, _, trackBackMap) -> Just (distance, trackBack trackBackMap finishPosition)
  where
    go ::
        -- | Distance from starting position
        Int ->
        -- | All the positions which we have visited and the distances they were reached on.
        Map position Int ->
        -- | Frontier subset of visited positions and the distances they were reached on.
        -- We only consider moves from here. Up to longestMove steps behind
        Map position Int ->
        -- Map for back-tracking, holds the incoming position for each visited position
        Map position (Maybe position) ->
        Maybe (Int, Map position Int, Map position Int, Map position (Maybe position))
    go distance visited frontier traceBack = case Map.lookup finishPosition frontier of
        Just finishDistance -> Just (finishDistance, visited, frontier, traceBack)
        Nothing ->
            if Map.null frontier
                then Nothing
                else go (distance + 1) visited' frontier' traceBack'
      where
        movesFromFrontier = L.foldl' addNextPositions Map.empty $ Map.toList frontier
        visited' = visited `Map.union` Map.map fst movesFromFrontier
        frontier' = Map.filter (> distance - longestMove) visited'
        traceBack' = traceBack `Map.union` Map.map snd movesFromFrontier
        addNextPositions :: Map position (Int, Maybe position) -> (position, Int) -> Map position (Int, Maybe position)
        addNextPositions m (pos, dist) = Map.union m nextPositions
          where
            nextPositions = Map.fromList . map (,(distance, Just pos)) $ moves (distance - dist) pos

-- Construct backward path from finish to start from backtrack map
trackBack :: Ord position => Map position (Maybe position) -> position -> [position]
trackBack m p = case Map.lookup p m of
    Just (Just p') -> p : trackBack m p'
    Just Nothing -> [p]
    Nothing -> error "Lost our way on trackback"
