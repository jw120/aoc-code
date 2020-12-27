{-# LANGUAGE ScopedTypeVariables #-}

module AOC_2018_15 where

import Control.Monad (foldM, when)
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import Data.List (nub, partition)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S

--
-- State
--

data State = State
  { cavern :: Array Coord Bool, -- False for walls. Coords (x from left, y from top) (0-indexed)
    mobs :: Map Coord Mob,
    finished :: Bool,
    elfPower :: Int
  }
  deriving (Eq)

instance Show State where
  show s = init $ unlines [row y | y <- [0 .. coordY cMax]]
    where
      (Coord {coordX = 0, coordY = 0}, cMax) = A.bounds (cavern s)
      row :: Int -> String
      row j = [toChar (isWall x) (hasMob x) | x <- [0 .. coordX cMax]]
        where
          c x = Coord {coordX = x, coordY = j}
          isWall x = cavern s A.! c x
          hasMob x = c x `M.lookup` mobs s
      toChar :: Bool -> Maybe Mob -> Char
      toChar False _ = '#'
      toChar True (Just mob) = if side mob == Elf then 'E' else 'G'
      toChar True Nothing = '.'

-- Coord has Ord instance to give "reading order"
data Coord = Coord {coordX :: Int, coordY :: Int} deriving (Eq, A.Ix)

cZero :: Coord
cZero = Coord {coordX = 0, coordY = 0}

instance Ord Coord where
  compare c1 c2 = case compare (coordY c1) (coordY c2) of
    LT -> LT
    GT -> GT
    EQ -> compare (coordX c1) (coordX c2)

instance Show Coord where
  show c = "(" ++ show (coordX c) ++ "," ++ show (coordY c) ++ ")"

data Side = Elf | Goblin deriving (Eq, Show)

data Mob = Mob
  { side :: Side,
    health :: Int
  }
  deriving (Eq, Show)

initialHealth :: Int
initialHealth :: Int = 200

--
-- Mob update logic
--

-- | Update the given mob
update :: Bool -> State -> Coord -> IO State
update logging s c = case M.lookup c (mobs s) of
  -- Catch the case where a mob is killed before it has a chance to move
  Nothing -> do
    when logging $ putStrLn ("No mob to move at " ++ show c)
    return s
  Just _mob -> do
    when logging $ putStrLn ("update on " ++ show c)
    when logging $ print s
    -- Move phase: move if no enemies adjacent
    let enemyNeigbours = adjacentEnemies s c
    (s', c') <-
      if null enemyNeigbours
        then move logging s c
        else return (s, c)
    -- Attack phase
    let enemyNeighbours' = adjacentEnemies s' c'
    if finished s' || null enemyNeighbours'
      then return s'
      else do
        let target = leastHP s enemyNeighbours'
        when logging $ putStrLn ("Attacking with " ++ show c' ++ " to " ++ show target)
        return $ attack s' target

-- | Move the mob at given coordinates
move :: Bool -> State -> Coord -> IO (State, Coord)
move logging s c = do
  let mob = fromJust $ M.lookup c (mobs s)
  when logging $ putStrLn ("Moving " ++ show (side mob) ++ " from " ++ show c)
  let enemyPositions = map fst . M.toList . M.filter ((/= side mob) . side) $ mobs s
  if null enemyPositions
    then return (s {finished = True}, c)
    else do
      when logging $ putStrLn ("Enemies: " ++ show enemyPositions)
      let openPositions = nub . filter (isOpen s) $ concatMap adjacent enemyPositions
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
              return (s {mobs = M.insert firstStep mob (M.delete c (mobs s))}, firstStep)

-- | Make an attack against the mob at given coordinates
attack :: State -> Coord -> State
attack s c
  | hp > 0 = s {mobs = M.insert c (targetMob {health = hp}) (mobs s)}
  | otherwise = s {mobs = M.delete c (mobs s)}
  where
    targetMob :: Mob = mobs s M.! c
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
        s' <- foldM (update logging) s (M.keys (mobs s))
        go (i + 1) s'

-- | Part (a) Update to steady state. Return number of rounds, elves health, goblins health
runToSteadyState :: Bool -> State -> IO (Int, [Int], [Int])
runToSteadyState logging state = toScore <$> go (0, state)
  where
    toScore :: (Int, State) -> (Int, [Int], [Int])
    toScore (i, s) = (i - 1, map health es, map health gs)
      where
        (es, gs) = partition ((== Elf) . side) $ M.elems (mobs s)
    go :: (Int, State) -> IO (Int, State)
    go (i, s)
      | finished s = return (i, s)
      | otherwise = do
        s' <- foldM (update logging) s (M.keys (mobs s))
        go (i + 1, s')

-- | Part (b) Increase elf attack power until no elves die
rampElfPower :: Bool -> State -> IO (Int, [Int])
rampElfPower logging s = go 3
  where
    allElves :: [Int] = map health . filter ((== Elf) . side) . M.elems $ mobs s
    go :: Int -> IO (Int, [Int])
    go p = do
      (n, elves, _) <- runToSteadyState logging (s {elfPower = p})
      if length elves == length allElves
        then return (n, elves)
        else go (p + 1)

-- | Return all adjacent coordinates
--
-- >>> sort . adjacent $ Coord { coordX = 2, coordY = 1 }
-- [(2,0),(1,1),(3,1),(2,2)]
adjacent :: Coord -> [Coord]
adjacent c =
  [ c {coordY = coordY c - 1},
    c {coordX = coordX c + 1},
    c {coordX = coordX c - 1},
    c {coordY = coordY c + 1}
  ]

-- | Return set of point adjacents to the given set
--
-- >>> sort . S.toList . adjacentSet $ S.fromList [Coord { coordX = 3, coordY = 3 }, Coord { coordX = 3, coordY = 4 }]
-- [(3,2),(2,3),(4,3),(2,4),(4,4),(3,5)]
adjacentSet :: Set Coord -> Set Coord
adjacentSet s = S.difference neighbours s
  where
    neighbours = S.unions . map (S.fromList . adjacent) $ S.toList s

-- | Return a sorted list of all adjacent enemies
adjacentEnemies :: State -> Coord -> [Coord]
adjacentEnemies s c = filter ((/= cSide) . side . (mobs s M.!)) adjacentAll
  where
    cSide :: Side = side $ mobs s M.! c
    adjacentAll :: [Coord] = filter (`M.member` mobs s) $ adjacent c

-- | Return coordinate of mob with lowest HP (tie broken by sort order)
leastHP :: State -> [Coord] -> Coord
leastHP s cs = head $ filter ((== minHP) . mobHealth) cs
  where
    mobHealth :: Coord -> Int
    mobHealth c = health $ mobs s M.! c
    minHP :: Int = minimum $ map mobHealth cs

-- | return the target(s) that are closest to the start point
findClosest :: State -> Coord -> [Coord] -> [Coord]
findClosest s start targets = go initialVisited initialFrontier
  where
    initialVisited :: Set Coord = S.empty
    initialFrontier :: Set Coord = S.singleton start
    go :: Set Coord -> Set Coord -> [Coord]
    go v f
      | S.null f = []
      | S.null i = go v' f'
      | otherwise = S.toList i
      where
        i = S.intersection f (S.fromList targets)
        v' = S.union v f
        f' = S.difference (S.filter (isOpen s) (adjacentSet f)) v'

-- | Is the given coordinate open (i.e. not a mob or wall)
isOpen :: State -> Coord -> Bool
isOpen s c = cavern s A.! c && M.notMember c (mobs s)

-- | Read State from input
--
-- >>> test1
-- #######
-- #.G.E.#
-- #E.G.E#
-- #.G.E.#
-- #######
readState :: [String] -> State
readState xs =
  State
    { cavern = A.array (cZero, Coord {coordX = xMax, coordY = yMax}) cavernData,
      mobs = M.fromList mobsData,
      finished = False,
      elfPower = 3
    }
  where
    (xMax, yMax) = (length (head xs) - 1, length xs - 1)
    tiles :: [(Coord, Char)] = zip [Coord {coordX = x, coordY = y} | y <- [0 .. yMax], x <- [0 .. xMax]] (concat xs)
    cavernData :: [(Coord, Bool)] = map (\(c, x) -> (c, x `elem` ".GE")) tiles
    mobsData :: [(Coord, Mob)] = mapMaybe toMob tiles
    toMob :: (Coord, Char) -> Maybe (Coord, Mob)
    toMob (c, 'E') = Just (c, Mob {side = Elf, health = initialHealth})
    toMob (c, 'G') = Just (c, Mob {side = Goblin, health = initialHealth})
    toMob _ = Nothing

--
-- Test Data
--

test1 :: State
test1 =
  readState
    [ "#######",
      "#.G.E.#",
      "#E.G.E#",
      "#.G.E.#",
      "#######"
    ]

test2 :: State
test2 =
  readState
    [ "#######",
      "#E..G.#",
      "#...#.#",
      "#.G.#G#",
      "#######"
    ]

test3 :: State
test3 =
  readState
    [ "#########",
      "#G..G..G#",
      "#.......#",
      "#.......#",
      "#G..E..G#",
      "#.......#",
      "#.......#",
      "#G..G..G#",
      "#########"
    ]

-- | Fourth set of test data
--
-- >>> do; s<- updateAll False 2 test4; putStrLn (hpSum s)
-- E: [188,194], G: [200,200,194,194]
-- >>> do; s<- updateAll False 23 test4; putStrLn (hpSum s)
-- E: [131], G: [200,200,131,131]
-- >>> do; s<- updateAll False 28 test4; putStrLn (hpSum s)
-- E: [113], G: [200,131,116,200]
-- >>> do; s<- updateAll False 47 test4; putStrLn (hpSum s)
-- E: [], G: [200,131,59,200]
-- >>> runToSteadyState False test4
-- (47,[],[200,131,59,200])
test4 :: State
test4 =
  readState
    [ "#######",
      "#.G...#",
      "#...EG#",
      "#.#.#G#",
      "#..G#E#",
      "#.....#",
      "#######"
    ]

-- | Fifth test
--
-- >>> runToSteadyState False test5
-- (37,[200,197,185,200,200],[])
test5 :: State
test5 =
  readState
    [ "#######",
      "#G..#E#",
      "#E#E.E#",
      "#G.##.#",
      "#...#E#",
      "#...E.#",
      "#######"
    ]

-- | Sixth test
--
-- >>> runToSteadyState False test6
-- (46,[164,197,200,98,200],[])
test6 :: State
test6 =
  readState
    [ "#######",
      "#E..EG#",
      "#.#G.E#",
      "#E.##E#",
      "#G..#.#",
      "#..E#.#",
      "#######"
    ]

-- | Seventh test
--
-- >>> runToSteadyState False test7
-- (35,[],[200,98,200,95,200])
test7 :: State
test7 =
  readState
    [ "#######",
      "#E.G#.#",
      "#.#G..#",
      "#G.#.G#",
      "#G..#.#",
      "#...E.#",
      "#######"
    ]

-- | Eighth test
--
-- >>> runToSteadyState False test8
-- (54,[],[200,98,38,200])
test8 :: State
test8 =
  readState
    [ "#######",
      "#.E...#",
      "#.#..G#",
      "#.###.#",
      "#E#G#G#",
      "#...#G#",
      "#######"
    ]

-- | Ninth test
--
-- >>> runToSteadyState False test9
-- (20,[],[137,200,200,200,200])
test9 :: State
test9 =
  readState
    [ "#########",
      "#G......#",
      "#.E.#...#",
      "#..##..G#",
      "#...##..#",
      "#...#...#",
      "#.G...G.#",
      "#.....G.#",
      "#########"
    ]

-- Show summary of mob HP
hpSum :: State -> String
hpSum s = "E: " ++ show (sideHealth Elf) ++ ", G: " ++ show (sideHealth Goblin)
  where
    sideHealth :: Side -> [Int]
    sideHealth x = map health . filter ((== x) . side) . M.elems $ mobs s

--
-- Main
--

main :: IO ()
main = do
  initialState <- readState . lines <$> getContents
  (aNum, aEs, aGs) <- runToSteadyState False initialState
  print $ aNum * sum (aEs ++ aGs)
  (bNum, bEs) <- rampElfPower False initialState
  print $ bNum * sum bEs
