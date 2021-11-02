{- |
 Module      : AOC_2018_15
 Description : Advent of code 2018 day 15
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_15 (solvers) where

import Control.Monad qualified as Mon (foldM, when)
import Data.Array.IArray (Array, Ix)
import Data.Array.IArray qualified as A (array, bounds, (!))
import Data.List qualified as L (nub, partition)
import Data.Map (Map)
import Data.Map qualified as Map (delete, elems, filter, fromList, insert, keys, lookup, member, notMember, toList, (!))
import Data.Maybe qualified as Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set (difference, empty, filter, fromList, intersection, null, singleton, toList, union, unions)
import Data.Text (Text)
import Data.Text qualified as T (lines, pack)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ aNum * sum (aEs ++ aGs)
    , T.pack . show $ bNum * sum bEs
    )
  where
    initialState = readState $ T.lines t
    (aNum, aEs, aGs) = runToSteadyState initialState
    (bNum, bEs) = rampElfPower initialState

--
-- State
--

data State = State
    { cavern :: Array Coord Bool -- False for walls. Coords (x from left, y from top) (0-indexed)
    , mobs :: Map Coord Mob
    , finished :: Bool
    , elfPower :: Int
    }
    deriving (Eq)

instance Show State where
    show s = init $ unlines [row y | y <- [0 .. coordY cMax]]
      where
        (_, cMax) = A.bounds (cavern s)
        row :: Int -> String
        row j = [toChar (isWall x) (hasMob x) | x <- [0 .. coordX cMax]]
          where
            c x = Coord{coordX = x, coordY = j}
            isWall x = cavern s A.! c x
            hasMob x = c x `Map.lookup` mobs s
        toChar :: Bool -> Maybe Mob -> Char
        toChar False _ = '#'
        toChar True (Just mob) = if side mob == Elf then 'E' else 'G'
        toChar True Nothing = '.'

-- Coord has Ord instance to give "reading order"
data Coord = Coord {coordX :: Int, coordY :: Int} deriving (Eq, Ix)

cZero :: Coord
cZero = Coord{coordX = 0, coordY = 0}

instance Ord Coord where
    compare c1 c2 = case compare (coordY c1) (coordY c2) of
        LT -> LT
        GT -> GT
        EQ -> compare (coordX c1) (coordX c2)

instance Show Coord where
    show c = "(" ++ show (coordX c) ++ "," ++ show (coordY c) ++ ")"

data Side = Elf | Goblin deriving (Eq, Show)

data Mob = Mob
    { side :: Side
    , health :: Int
    }
    deriving (Eq, Show)

initialHealth :: Int
initialHealth :: Int = 200

--
-- Mob update logic
--

-- | Update the given mob
update :: Bool -> State -> Coord -> State
update logging s c = case Map.lookup c (mobs s) of
    -- Catch the case where a mob is killed before it has a chance to move
    Nothing -> s
    Just _mob -> let enemyNeighbors = adjacentEnemies s c
                     (s', c') = if null enemyNeighbors then move logging s c else (s, c)
                     enemyNeighbors' = adjacentEnemies s' c'
        if finished s' || null enemyNeighbors'
            then return s'
            else do
                let target = leastHP s enemyNeighbors'
                when logging $ putStrLn ("Attacking with " ++ show c' ++ " to " ++ show target)
                return $ attack s' target

-- | Move the mob at given coordinates
move :: Bool -> State -> Coord -> IO (State, Coord)
move logging s c = do
    let mob = Maybe.fromJust $ Map.lookup c (mobs s)
    when logging $ putStrLn ("Moving " ++ show (side mob) ++ " from " ++ show c)
    let enemyPositions = map fst . Map.toList . Map.filter ((/= side mob) . side) $ mobs s
    if null enemyPositions
        then return (s{finished = True}, c)
        else do
            when logging $ putStrLn ("Enemies: " ++ show enemyPositions)
            let openPositions = L.nub . filter (isOpen s) $ concatMap adjacent enemyPositions
            when logging $ putStrLn ("Open positions: " ++ show openPositions)
            if null openPositions
                then return (s, c)
                else do
                    let closestPositions = findClosest s c openPositions
                    when logging $ putStrLn ("Closest positions: " ++ show closestPositions)
                    if null closestPositions -- no reachable position
                        then return (s, c)
                        else do
                            let destination = minimum closestPositions
                            when logging $ putStrLn ("Chosen position: " ++ show destination)
                            let closestFirstSteps = findClosest s destination . filter (isOpen s) $ adjacent c
                            when logging $ putStrLn ("First steps closest to destination: " ++ show closestFirstSteps)
                            let firstStep = minimum closestFirstSteps
                            when logging $ putStrLn ("First step: " ++ show firstStep)
                            return (s{mobs = Map.insert firstStep mob (Map.delete c (mobs s))}, firstStep)

-- | Make an attack against the mob at given coordinates
attack :: State -> Coord -> State
attack s c
    | hp > 0 = s{mobs = Map.insert c (targetMob{health = hp}) (mobs s)}
    | otherwise = s{mobs = Map.delete c (mobs s)}
  where
    targetMob :: Mob = mobs s Map.! c
    attackPower :: Int = if side targetMob == Elf then 3 else elfPower s
    hp :: Int = health targetMob - attackPower

-- | Update every unit n times
updateAll :: Bool -> Int -> State -> IO State
updateAll logging n = go 0
  where
    go :: Int -> State -> IO State
    go i s
        | i >= n = return s
        | otherwise = do
            s' <- Mon.foldM (update logging) s (Map.keys (mobs s))
            go (i + 1) s'

-- | Part (a) Update to steady state. Return number of rounds, elves health, goblins health
runToSteadyState :: Bool -> State -> IO (Int, [Int], [Int])
runToSteadyState logging state = toScore <$> go (0, state)
  where
    toScore :: (Int, State) -> (Int, [Int], [Int])
    toScore (i, s) = (i - 1, map health es, map health gs)
      where
        (es, gs) = L.partition ((== Elf) . side) $ Map.elems (mobs s)
    go :: (Int, State) -> IO (Int, State)
    go (i, s)
        | finished s = return (i, s)
        | otherwise = do
            s' <- Mon.foldM (update logging) s (Map.keys (mobs s))
            go (i + 1, s')

-- | Part (b) Increase elf attack power until no elves die
rampElfPower :: Bool -> State -> IO (Int, [Int])
rampElfPower logging s = go 3
  where
    allElves :: [Int] = map health . filter ((== Elf) . side) . Map.elems $ mobs s
    go :: Int -> IO (Int, [Int])
    go p = do
        (n, elves, _) <- runToSteadyState logging (s{elfPower = p})
        if length elves == length allElves
            then return (n, elves)
            else go (p + 1)

-- | Return all adjacent coordinates
adjacent :: Coord -> [Coord]
adjacent c =
    [ c{coordY = coordY c - 1}
    , c{coordX = coordX c + 1}
    , c{coordX = coordX c - 1}
    , c{coordY = coordY c + 1}
    ]

-- | Return set of point adjacent to the given set
adjacentSet :: Set Coord -> Set Coord
adjacentSet s = Set.difference neighbors s
  where
    neighbors = Set.unions . map (Set.fromList . adjacent) $ Set.toList s

-- | Return a sorted list of all adjacent enemies
adjacentEnemies :: State -> Coord -> [Coord]
adjacentEnemies s c = filter ((/= cSide) . side . (mobs s Map.!)) adjacentAll
  where
    cSide :: Side = side $ mobs s Map.! c
    adjacentAll :: [Coord] = filter (`Map.member` mobs s) $ adjacent c

-- | Return coordinate of mob with lowest HP (tie broken by sort order)
leastHP :: State -> [Coord] -> Coord
leastHP s cs = head $ filter ((== minHP) . mobHealth) cs
  where
    mobHealth :: Coord -> Int
    mobHealth c = health $ mobs s Map.! c
    minHP :: Int = minimum $ map mobHealth cs

-- | return the target(s) that are closest to the start point
findClosest :: State -> Coord -> [Coord] -> [Coord]
findClosest s start targets = go initialVisited initialFrontier
  where
    initialVisited :: Set Coord = Set.empty
    initialFrontier :: Set Coord = Set.singleton start
    go :: Set Coord -> Set Coord -> [Coord]
    go v f
        | Set.null f = []
        | Set.null i = go v' f'
        | otherwise = Set.toList i
      where
        i = Set.intersection f (Set.fromList targets)
        v' = Set.union v f
        f' = Set.difference (Set.filter (isOpen s) (adjacentSet f)) v'

-- | Is the given coordinate open (i.e. not a mob or wall)
isOpen :: State -> Coord -> Bool
isOpen s c = cavern s A.! c && Map.notMember c (mobs s)

-- | Read State from input
readState :: [String] -> State
readState xs =
    State
        { cavern = A.array (cZero, Coord{coordX = xMax, coordY = yMax}) cavernData
        , mobs = Map.fromList mobsData
        , finished = False
        , elfPower = 3
        }
  where
    (xMax, yMax) = (length (head xs) - 1, length xs - 1)
    tiles :: [(Coord, Char)] = zip [Coord{coordX = x, coordY = y} | y <- [0 .. yMax], x <- [0 .. xMax]] (concat xs)
    cavernData :: [(Coord, Bool)] = map (\(c, x) -> (c, x `elem` ".GE")) tiles
    mobsData :: [(Coord, Mob)] = mapMaybe toMob tiles
    toMob :: (Coord, Char) -> Maybe (Coord, Mob)
    toMob (c, 'E') = Just (c, Mob{side = Elf, health = initialHealth})
    toMob (c, 'G') = Just (c, Mob{side = Goblin, health = initialHealth})
    toMob _ = Nothing

-- Show summary of mob HP
hpSum :: State -> String
hpSum s = "E: " ++ show (sideHealth Elf) ++ ", G: " ++ show (sideHealth Goblin)
  where
    sideHealth :: Side -> [Int]
    sideHealth x = map health . filter ((== x) . side) . Map.elems $ mobs s
