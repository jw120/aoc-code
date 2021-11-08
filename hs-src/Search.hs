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
import Data.Map qualified as Map (empty, fromList, keys, keysSet, lookup, singleton, union)
import Data.Set (Set)
import Data.Set qualified as Set (difference, fromList, member, null, singleton, toList, union)

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
    -- | Returns one of the shortest paths from start to finish if one exists
    Maybe (Int, [position])
bfsBasic moves finishPosition startPosition =
    case go (0, Set.singleton startPosition, Set.singleton startPosition, Map.singleton startPosition Nothing) of
        Nothing -> Nothing
        Just (distance, _, _, backtrackMap) -> Just $ (distance, traceBack finishPosition)
          where
            traceBack :: position -> [position]
            traceBack p = case Map.lookup p backtrackMap of
                Just (Just p') -> p : traceBack p'
                Just Nothing -> [p]
                Nothing -> error "Lost our way on trackback"
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
        | otherwise = go (distance + 1, visited', frontier', traceBack')
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
    -- | Returns one of the shortest paths from start to finish if one exists
    Maybe (Int, [position])
bfsVariable moves longestMove finishPosition startPosition =
    case go (0, Set.singleton startPosition, Set.singleton startPosition, Map.singleton startPosition Nothing) of
        Nothing -> Nothing
        Just (distance, _, _, backtrackMap) -> Just $ (distance, traceBack finishPosition)
          where
            traceBack :: position -> [position]
            traceBack p = case Map.lookup p backtrackMap of
                Just (Just p') -> p : traceBack p'
                Just Nothing -> [p]
                Nothing -> error "Lost our way on trackback"
  where
    go ::
        -- | Distance from starting position
        Int ->
        -- | All the positions which we have visited
        Set position ->
        -- | Frontier subset of visited positions - only consider moves from here. Up to longestMove steps behind
        Set position ->
        -- Map for back-tracking, holds the incoming position for each visited position
        Map position (Maybe position) ->
        Maybe (Int, Set position, Set position, Map position (Maybe position))

        (Int, Set position, Set position, Map position (Maybe position)) ->
        Maybe (Int, Set position, Set position, Map position (Maybe position))
    go (distance, lastChangeDistance, visited, frontier, traceBack) -- frontier is a subset of (the keys in) visited
        | finishPosition `Set.member` frontier = Just (distance, lastChangeDistance, visited, frontier, traceBack)
        | distance > lastChangeDistance + longestMove = Nothing
        | otherwise = go (distance + 1, lastChangeDistance, visited', frontier', traceBack')
      where
        movesFromFrontier = L.foldl' addNextPositions Map.empty $ Set.toList frontier
        frontier' = Set.fromList (Map.keys movesFromFrontier) `Set.difference` visited
        visited' = visited `Set.union` Map.keysSet movesFromFrontier
        traceBack' = traceBack `Map.union` movesFromFrontier
        lastChange
        addNextPositions :: Map position (Maybe position) -> position -> Map position (Maybe position)
        addNextPositions m pos = Map.union m nextPositions
          where
            nextPositions = Map.fromList . map (,Just pos) $ moves (distance - lastChangeDistance) pos

{-

{- | Simple breadth-first search with single-distance steps and potentially multiple end-states
 Returns first final state found and map for backtracking
-}
bfs ::
    forall position.
    Ord position =>
    -- | Function to generates list of moves available from current position that are within the given distance
    (position -> [position]) ->
    -- | Function to test if the current position is a finish state
    (position -> Bool) ->
    -- | Starting state
    position ->
    -- | Returns least-distance final positions and distance from the start and a map that gives (one of) the positions
    -- from which that position is reached
    Maybe (Map position (Maybe position), Set position)
bfs moves isFinish startPosition = go (Map.singleton startPosition Nothing, Set.singleton startPosition)
  where
    go ::
        (Map position (Maybe position), Set position) ->
        Maybe (Map position (Maybe position), Set position)
    go (visited, frontier) -- frontier is a subset of (the keys in) visited
        | not (Set.null finishedFrontier) = Just (visited, finishedFrontier)
        | otherwise = go (newVisited, newFrontier)
      where
        finishedFrontier :: Set position = Set.filter isFinish frontier
        newVisited :: Map position (Maybe position) = L.foldl' addNextPositions Map.empty $ Set.toList frontier
        addNextPositions :: Map position (Maybe position) -> position -> Map position (Maybe position)
        addNextPositions m pos = Map.union m nextPositions
            where nextPositions = Map.fromList . map (\next -> (next, Just pos)) $ moves pos
        beyondFrontier = Set.fromList . concatMap . map (\pos -> (pos, moves pos)) $ Set.toList frontier
        newFrontier = beyondFrontier `Set.difference` Map.keysSet visited
        newFrontierMap = Map.fromList $ map (\pos -> (pos, newFrontier
        newVisited = visited

{-
f= {0} v = {}
f = {1, 2}. v = {}

-}

-- case moves (pos, dist) of
-- [] -> Nothing
-- (nextPos, nextDist) : _ -> case Map.lookup nextPos visited of
--     Nothing ->

--     go ((pos', dist'), visited')
--     where
--         visited' = Map.insert

-}