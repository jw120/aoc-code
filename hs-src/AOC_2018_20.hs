{- |
 Module      : AOC_2018_20
 Description : Advent of code 2018 day 20
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_20 (solvers) where

import Data.List qualified as L (foldl')
import Data.Maybe qualified as Maybe (fromJust, isJust)
import Data.Set (Set)
import Data.Set qualified as Set (
    empty,
    filter,
    findMax,
    findMin,
    fromList,
    insert,
    map,
    member,
    notMember,
    null,
    singleton,
    size,
    toList,
    union,
 )

import Data.Text (Text)
import Data.Text qualified as T (cons, head, init, last, null, pack, tail, uncons, unpack)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ flood doors
    , T.pack . show $ remaining 1000 doors
    )
  where
    doors = readPath . T.tail . T.init $ T.init t -- trimming trailing newline and $..^

type Position = (Int, Int)

origin :: Position
origin = (0, 0)

data Direction = North | East | South | West deriving (Eq, Ord)

instance Show Direction where
    show North = "N"
    show South = "S"
    show East = "E"
    show West = "W"

step :: Direction -> Position -> Position
North `step` (x, y) = (x, y + 1)
South `step` (x, y) = (x, y - 1)
East `step` (x, y) = (x + 1, y)
West `step` (x, y) = (x - 1, y)

{- | Parse a regex path and return a set of open doors

 We index each grid square by its (left-right, down-up) coordinates with (0,0) as the starting
 square. Then we hold the partially generated map as a set of doors which are known to be open
-}
readPath :: Text -> Set Position
readPath s = openDoors
  where
    (_, openDoors, _) = go (Set.singleton origin, Set.empty, origin) s
    go :: (Set Position, Set Position, Position) -> Text -> (Set Position, Set Position, Position)
    go state@(_visitedRooms, _openDoors, pos) t = case T.uncons t of
        Just ('N', rest) -> go (walkStep state North) rest
        Just ('S', rest) -> go (walkStep state South) rest
        Just ('E', rest) -> go (walkStep state East) rest
        Just ('W', rest) -> go (walkStep state West) rest
        Just ('(', rest) -> (visitedRooms'', openDoors'', pos')
          where
            (firstBranch, otherBranches, postBranch) = splitBranches $ T.unpack rest
            (visitedRooms', openDoors', pos') = go state (firstBranch <> postBranch)
            (visitedRooms'', openDoors'') = L.foldl' backtrack (visitedRooms', openDoors') otherBranches
            backtrack :: (Set Position, Set Position) -> Text -> (Set Position, Set Position)
            backtrack (rooms, doors) st
                | p' `Set.member` rooms = (rooms', doors') -- skip postBranch if we end up in an existing room
                | otherwise = (rooms'', doors'')
              where
                (rooms', doors', p') = go (rooms, doors, pos) st
                (rooms'', doors'', _) = go (rooms', doors', p') postBranch
        Just (c, rest) -> error . T.unpack $ "Unexpected character at '" <> (c `T.cons` rest) <> "'"
        Nothing -> state

-- | Split branches from a regex string
splitBranches :: String -> (Text, [Text], Text)
splitBranches s = case fst $ go 0 (([], ""), s) of
    (x : xs, y) -> (T.pack x, map T.pack xs, T.pack y)
    ([], _) -> error "Empty in splitBranches"
  where
    go :: Int -> (([String], String), String) -> (([String], String), String)
    go 0 ((acc, current), '|' : rest) = go 0 ((acc ++ [current], ""), rest)
    go 0 ((acc, current), ')' : rest) = ((acc ++ [current], rest), "")
    go n ((acc, current), ')' : rest) = go (n - 1) ((acc, current ++ ")"), rest)
    go n ((acc, current), '(' : rest) = go (n + 1) ((acc, current ++ "("), rest)
    go n ((acc, current), c : rest) = go n ((acc, current ++ [c]), rest)
    go _ _ = error "Unexpected branch"

-- | Walk through door in a given direction from current position updating visited sets
walkStep :: (Set Position, Set Position, Position) -> Direction -> (Set Position, Set Position, Position)
walkStep (visitedRooms, openDoors, p) d = (visitedRooms', openDoors', p2)
  where
    p1 = d `step` p
    p2 = d `step` p1
    openDoors' = p1 `Set.insert` openDoors
    visitedRooms' = p2 `Set.insert` visitedRooms

-- | Flood fill maze starting at origin, returning the number of steps that are needed to reach all rooms
flood :: Set Position -> Int
flood openDoors = floodGo (const False) id openDoors 0 Set.empty (Set.singleton origin)

-- | Flood fill maze starting at origin for given number of steps, returning number rooms reached
floodTo :: Int -> Set Position -> Int
floodTo n openDoors = floodGo (>= n) (\_ -> error "Unexpected end") openDoors 0 Set.empty (Set.singleton origin)

-- Shared flooding helper function, returns number of steps to complete flood fill or number of sites visited with limited number of steps
floodGo :: (Int -> Bool) -> (Int -> Int) -> Set Position -> Int -> Set Position -> Set Position -> Int
floodGo checkStepCount onNull openDoors stepCount visited boundary = case (checkStepCount stepCount, Set.null boundary') of
    (True, _) -> Set.size visited
    (_, True) -> onNull stepCount
    _ -> floodGo checkStepCount onNull openDoors (stepCount + 1) visited' boundary'
  where
    -- update the set of visited rooms
    visited' = visited `Set.union` boundary
    -- update the boundary with adjacent cells that connect via open doors and are unvisited
    adjacents :: Set (Position, Direction)
    adjacents = Set.fromList [(p, d) | p <- Set.toList boundary, d <- [North, East, South, West]]
    isNew :: (Position, Direction) -> Maybe Position
    isNew (p, d)
        | p1 `Set.notMember` openDoors = Nothing
        | p2 `Set.member` visited' = Nothing
        | otherwise = Just p2
      where
        p1 = d `step` p
        p2 = d `step` p1
    boundary' = Set.map Maybe.fromJust . Set.filter Maybe.isJust $ Set.map isNew adjacents

-- | Number of rooms remaining unvisited after n steps from start
remaining :: Int -> Set Position -> Int
remaining n doors = totalRooms - floodTo n doors
  where
    totalRooms = xRoomCount * yRoomCount
    xDoorMin = Set.findMin $ Set.map fst doors
    xDoorMax = Set.findMax $ Set.map fst doors
    yDoorMin = Set.findMin $ Set.map snd doors
    yDoorMax = Set.findMax $ Set.map snd doors
    xRoomCount = 1 + (xDoorMax - xDoorMin) `div` 2
    yRoomCount = 1 + (yDoorMax - yDoorMin) `div` 2

-- | Generate a set of the positions adjacent to the given position
neighbours :: Position -> Set Position
neighbours p = Set.fromList [step North p, step East p, step South p, step West p]

-- | Call the solver
runRegex :: Text -> Int
runRegex = flood . parseRegex

parseRegex :: Text -> Set Position
parseRegex s
    | T.null s = error "Null string"
    | T.head s /= '^' = error "Missing ^"
    | T.last s /= '$' = error "Missing $"
    | otherwise = readPath . T.init $ T.tail s
