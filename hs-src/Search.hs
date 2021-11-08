{- |
 Module      : Search
 Description : Shared code for BFS
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module Search (bfsBasic) where

import Data.List qualified as L (foldl')
import Data.Map (Map)
import Data.Map qualified as Map (empty, fromList, keys, keysSet, lookup, singleton, union)
import Data.Set (Set)
import Data.Set qualified as Set (difference, fromList, member, null, singleton, toList, union)

-- | Basic breadth-first search with single-distance steps and single end-state. Returns path.
bfsBasic ::
    forall position.
    Ord position =>
    -- | Function to generates list of moves available from current position that are within the given distance
    (position -> [position]) ->
    -- | Finish state
    position ->
    -- | Starting state
    position ->
    -- | Returns one of the shortest paths from start to finish if one exists
    Maybe [position]
bfsBasic moves finishPosition startPosition =
    case go (Set.singleton startPosition, Set.singleton startPosition, Map.singleton startPosition Nothing) of
        Nothing -> Nothing
        Just (_, _, backtrackMap) -> Just $ traceBack finishPosition
          where
            traceBack :: position -> [position]
            traceBack p = case Map.lookup p backtrackMap of
                Just (Just p') -> p : traceBack p'
                Just Nothing -> [p]
                Nothing -> error "Lost our way on trackback"
  where
    go ::
        (Set position, Set position, Map position (Maybe position)) ->
        Maybe (Set position, Set position, Map position (Maybe position))
    go (visited, frontier, traceBack) -- frontier is a subset of (the keys in) visited
        | finishPosition `Set.member` frontier = Just (visited, frontier, traceBack)
        | Set.null frontier = Nothing
        | otherwise = go (visited', frontier', traceBack')
      where
        movesFromFrontier = L.foldl' addNextPositions Map.empty $ Set.toList frontier
        frontier' = Set.fromList (Map.keys movesFromFrontier) `Set.difference` visited
        visited' = visited `Set.union` Map.keysSet movesFromFrontier
        traceBack' = traceBack `Map.union` movesFromFrontier
        addNextPositions :: Map position (Maybe position) -> position -> Map position (Maybe position)
        addNextPositions m pos = Map.union m nextPositions
          where
            nextPositions = Map.fromList . map (,Just pos) $ moves pos

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