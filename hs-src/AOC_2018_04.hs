{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AOC_2018_04 where

import Data.List qualified as L (foldl', nub, sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T (lines, pack)
import Text.Megaparsec qualified as M (choice)
import Text.Megaparsec.Char qualified as MC (char, string)
import Utilities (Parser, lexeme, pUnsignedInt, parseOrStop, ($>))

solvers :: Text -> (Text, Text)
solvers input =
    ( T.pack $ show (sleepiestGuard * sleepiestMinute)
    , T.pack $ show (sleepiestPairGuard * sleepiestPairMinute)
    )
  where
    observations :: [ObsRecord] = map (parseOrStop pObsRecord) $ T.lines input
    sortedObservations :: [ObsRecord] = L.sortBy cmpObs observations
    cleanedObservations :: [ObsRecord] = moveEndDayGuards sortedObservations
    obsByDays :: [[Action]] = groupByDays cleanedObservations
    sleepIntervals :: [(Guard, Minute, Minute)] =
        concatMap toSleepIntervals obsByDays
    guardTotals :: Map Guard Minute = toGuardTotals sleepIntervals
    Guard sleepiestGuard :: Guard = keyOfMaxValue guardTotals
    sleepiestGuardIntervals :: [(Minute, Minute)] =
        map (\(_, t1, t2) -> (t1, t2)) $
            filter (\(Guard g, _, _) -> g == sleepiestGuard) sleepIntervals
    Minute sleepiestMinute :: Minute = maxMinute sleepiestGuardIntervals
    (Guard sleepiestPairGuard, Minute sleepiestPairMinute) :: (Guard, Minute) =
        maxPair sleepIntervals

newtype Guard = Guard Int deriving (Show, Ord, Eq)

newtype Minute = Minute Int deriving (Show, Ord, Eq) -- in range 0..59

newtype Month = Month Int deriving (Show, Ord, Eq) -- in range 1-12

newtype Day = Day Int deriving (Show, Ord, Eq) -- in range 1-31

-- Records of observations that we parse
data ObsRecord = ObsRecord Month Day ObsAction deriving (Show)

-- Actions that are part of our observations
data ObsAction
    = ObsWake Minute -- Guard wakes up
    | ObsSleep Minute -- Guard sleeps
    | ObsInPeriodGuardChange Guard Minute -- new guard
    | ObsEndDayGuardChange Guard -- new guard at end of the day (23:xx)
    deriving (Show)

-- compare observation records on the Minute for sorting
cmpObs :: ObsRecord -> ObsRecord -> Ordering
cmpObs (ObsRecord m1 d1 a1) (ObsRecord m2 d2 a2) = case compare m1 m2 of
    GT -> GT
    LT -> LT
    EQ -> case compare d1 d2 of
        GT -> GT
        LT -> LT
        EQ -> case compare (hour a1) (hour a2) of
            GT -> GT
            LT -> LT
            EQ -> compare (minute a1) (minute a2)
  where
    hour :: ObsAction -> Int
    hour (ObsEndDayGuardChange _) = 23
    hour _ = 0
    minute :: ObsAction -> Minute
    minute (ObsEndDayGuardChange _) =
        error "No minute kept for end-day guard change"
    minute (ObsWake t) = t
    minute (ObsSleep t) = t
    minute (ObsInPeriodGuardChange _ t) = t

-- | Parse an observation record
pObsRecord :: Parser ObsRecord
pObsRecord = do
    _ <- MC.char '[' *> pUnsignedInt -- ignore the year
    month <- Month <$> (MC.char '-' *> pUnsignedInt)
    day <- Day <$> (MC.char '-' *> pUnsignedInt)
    hour <- pUnsignedInt
    minute <- Minute <$> (MC.char ':' *> pUnsignedInt <* lexeme (MC.char ']'))
    M.choice
        [ MC.string "wakes up" $> ObsRecord month day (ObsWake minute)
        , MC.string "falls asleep" $> ObsRecord month day (ObsSleep minute)
        , do
            guard <- Guard <$> (MC.string "Guard #" *> pUnsignedInt <* MC.string "begins shift")
            if hour == 0
                then return (ObsRecord month day (ObsInPeriodGuardChange guard minute))
                else return (ObsRecord month day (ObsEndDayGuardChange guard))
        ]

-- Replace guard changes before midnight with midnight changes the day of the following record
moveEndDayGuards :: [ObsRecord] -> [ObsRecord]
moveEndDayGuards ((ObsRecord _ _ (ObsEndDayGuardChange g)) : r2@(ObsRecord m2 d2 _) : rest) =
    ObsRecord m2 d2 (ObsInPeriodGuardChange g (Minute 0)) :
    moveEndDayGuards (r2 : rest)
moveEndDayGuards (r1 : r2 : rest) = r1 : moveEndDayGuards (r2 : rest)
moveEndDayGuards x = x

-- Actions that we process after stripping timings
data Action
    = GuardStart Guard
    | Sleep Minute
    | Wake Minute
    deriving (Show)

groupByDays :: [ObsRecord] -> [[Action]]
groupByDays [] = []
groupByDays (first : rest) =
    map toDayAction (first : otherFirstDayObs) : groupByDays otherDays
  where
    (otherFirstDayObs, otherDays) = span (sameDay first) rest
    sameDay :: ObsRecord -> ObsRecord -> Bool
    sameDay (ObsRecord m1 d1 _) (ObsRecord m2 d2 _) = m1 == m2 && d1 == d2
    toDayAction :: ObsRecord -> Action
    toDayAction (ObsRecord _ _ (ObsWake t)) = Wake t
    toDayAction (ObsRecord _ _ (ObsSleep t)) = Sleep t
    toDayAction (ObsRecord _ _ (ObsInPeriodGuardChange g _)) = GuardStart g
    toDayAction (ObsRecord _ _ (ObsEndDayGuardChange _)) =
        error "Not expecting end-day"

-- process the actions to give sleep intervals
toSleepIntervals :: [Action] -> [(Guard, Minute, Minute)]
toSleepIntervals actions = xs
  where
    (_, _, xs) = L.foldl' f (Nothing, Nothing, []) actions
    f (Just g, Just t1, acc) (Wake t2) = (Just g, Nothing, acc ++ [(g, t1, t2)])
    f (Just g, Nothing, acc) (Sleep t) = (Just g, Just t, acc)
    f (_, _, acc) (GuardStart g) = (Just g, Nothing, acc)
    f x y = error $ "Unexpected pattern" ++ show (x, y)

-- sum all the sleeping time for each guard
toGuardTotals :: [(Guard, Minute, Minute)] -> Map Guard Minute
toGuardTotals = L.foldl' f Map.empty
  where
    f :: Map Guard Minute -> (Guard, Minute, Minute) -> Map Guard Minute
    f m (g, Minute t1, Minute t2) = Map.insertWith add g (Minute (t2 - t1)) m
    add :: Minute -> Minute -> Minute
    add (Minute t1) (Minute t2) = Minute (t1 + t2)

-- Return the key of the map with the highest value (or the first such key if more than one)
keyOfMaxValue :: Ord v => Map k v -> k
keyOfMaxValue m = case Map.foldlWithKey' f Nothing m of
    Just (key, _) -> key
    Nothing -> error "No key"
  where
    f :: Ord v => Maybe (k, v) -> k -> v -> Maybe (k, v)
    f (Just (maxG, maxT)) g t
        | t > maxT = Just (g, t)
        | otherwise = Just (maxG, maxT)
    f Nothing g t = Just (g, t)

-- which minute is included most often in the intervals
maxMinute :: [(Minute, Minute)] -> Minute
maxMinute = keyOfMaxValue . countMinutes
  where
    countMinutes :: [(Minute, Minute)] -> Map Minute Int
    countMinutes = L.foldl' addInterval zeroCounts
    addInterval :: Map Minute Int -> (Minute, Minute) -> Map Minute Int
    addInterval m (Minute t1, Minute t2) =
        L.foldl' addMinute m [Minute t | t <- [t1 .. t2 - 1]]
    addMinute :: Map Minute Int -> Minute -> Map Minute Int
    addMinute m t = Map.adjust (1 +) t m
    zeroCounts :: Map Minute Int
    zeroCounts = Map.fromList [(Minute t, 0) | t <- [0 .. 59]]

-- which guard,minute pair is most frequent
maxPair :: [(Guard, Minute, Minute)] -> (Guard, Minute)
maxPair triplets = keyOfMaxValue countMinutes
  where
    countMinutes :: Map (Guard, Minute) Int
    countMinutes = L.foldl' addInterval zeroCounts triplets
    addInterval ::
        Map (Guard, Minute) Int ->
        (Guard, Minute, Minute) ->
        Map (Guard, Minute) Int
    addInterval m (g, Minute t1, Minute t2) =
        L.foldl' (addMinute g) m [Minute t | t <- [t1 .. t2 - 1]]
    addMinute ::
        Guard -> Map (Guard, Minute) Int -> Minute -> Map (Guard, Minute) Int
    addMinute g m t = Map.adjust (1 +) (g, t) m
    zeroCounts :: Map (Guard, Minute) Int
    zeroCounts = Map.fromList [((g, Minute t), 0) | g <- guards, t <- [0 .. 59]]
    guards :: [Guard]
    guards = L.nub $ map (\(g, _, _) -> g) triplets
