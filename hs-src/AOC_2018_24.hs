{- |
 Module      : AOC_2018_24
 Description : Advent of code 2018 day 24
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_24 (solvers) where

import Data.Foldable qualified as Fold (maximumBy)
import Data.List qualified as L (foldl', sortBy)
import Data.Map (Map)
import Data.Map qualified as Map (elems, filter, fromList, insert, keys, map, size, (!))

import Data.Text (Text)
import Data.Text qualified as T (pack)
import Text.Megaparsec qualified as M (many, sepBy)
import Text.Megaparsec.Char qualified as MC (char, string)

import Utilities (Parser, lexeme, pUnsignedInt, parseOrStop, ($>), (<|>))

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ numUnits state'
    , T.pack . show $ numUnits state''
    )
  where
    state = parseOrStop pState t
    state' = runAll state
    state'' = snd $ boostUntilWin state

newtype GroupIndex = GroupIndex Int deriving (Eq, Ord)

instance Show GroupIndex where
    show (GroupIndex i) = show i

type State = Map GroupIndex Group

-- printState :: State -> IO ()
-- printState s = putStrLn $ "Immune System:\n" ++ showGroups immuneSystem ++ "Infection:\n" ++ showGroups infection
--   where
--     immuneSystem :: State = Map.filter ((== ImmuneSystem) . side) s
--     infection :: State = Map.filter ((== Infection) . side) s
--     showGroups :: State -> String
--     showGroups = unlines . map (\(GroupIndex i, g) -> pad i ++ show i ++ ": " ++ show g) . Map.assocs
--     pad x = if x < 10 then " " else ""

data Group = Group
    { n :: Int
    , hp :: Int
    , atk :: Int
    , atkType :: AtkType
    , weak :: [AtkType]
    , immune :: [AtkType]
    , initiative :: Int
    , side :: Side
    }

instance Show Group where
    show g =
        "  " ++ show (n g) ++ " x " ++ show (hp g)
            ++ " ("
            ++ show (atk g)
            ++ " "
            ++ show (atkType g)
            ++ " on "
            ++ show (initiative g)
            ++ ", "
            ++ show [weak g, immune g]
            ++ ", "
            ++ show (side g)
            ++ ")"

data AtkType = Cold | Fire | Bludgeoning | Slashing | Radiation deriving (Eq, Show)

data Side = ImmuneSystem | Infection deriving (Eq, Show)

-- | Read initial state
pState :: Parser State
pState = do
    immuneSystem <- lexeme (MC.string "Immune System:") *> M.many (lexeme (group ImmuneSystem))
    infection <- lexeme (MC.string "Infection:") *> M.many (lexeme (group Infection))
    return . Map.fromList $ zip (map GroupIndex [0 ..]) (immuneSystem ++ infection)

-- | Parse group
group :: Side -> Parser Group
group s = do
    pN <- pUnsignedInt
    pHP <- MC.string "units each with " *> pUnsignedInt
    (pWeak, pImmune) <- MC.string "hit points " *> lexeme weaknesses
    pAtk <- MC.string "with an attack that does " *> pUnsignedInt
    t <- attackType
    pInitiative <- MC.string " damage at initiative " *> pUnsignedInt
    return
        Group
            { n = pN
            , hp = pHP
            , atk = pAtk
            , atkType = t
            , initiative = pInitiative
            , weak = pWeak
            , immune = pImmune
            , side = s
            }

{- | Parse possible list of weaknesses and immunities

 >>> parseTest weaknesses $ pack "with an attack"
 Done "with an attack" ([],[])
 >>> parseTest weaknesses $ pack "(weak to bludgeoning; immune to slashing, fire)"
 Done "" ([Bludgeoning],[Slashing,Fire])
 >>> parseTest weaknesses $ pack "(weak to bludgeoning; immune to cold)"
 Done "" ([Bludgeoning],[Cold])
-}
weaknesses :: Parser ([AtkType], [AtkType])
weaknesses = weaknessList <|> return ([], [])
  where
    weaknessList :: Parser ([AtkType], [AtkType])
    weaknessList = combineClauses <$> (MC.char '(' *> M.many weaknessClause <* MC.char ')')
    weaknessClause :: Parser (Bool, [AtkType])
    weaknessClause = do
        isImmune <- (MC.string "immune to " $> True) <|> (MC.string "weak to " $> False)
        types <- M.sepBy attackType ", "
        _ <- lexeme . M.many $ MC.char ';'
        return (isImmune, types)
    combineClauses :: [(Bool, [AtkType])] -> ([AtkType], [AtkType])
    combineClauses [(True, xs)] = ([], xs)
    combineClauses [(False, xs)] = (xs, [])
    combineClauses [(True, xs), (False, ys)] = (ys, xs)
    combineClauses [(False, xs), (True, ys)] = (xs, ys)
    combineClauses x = error $ "Bad clauses " ++ show x

{- | Parse an attack type

 >>> parseTest attackType $ pack "fire"
 Done "" Fire
-}
attackType :: Parser AtkType
attackType =
    MC.string "cold" $> Cold
        <|> MC.string "fire" $> Fire
        <|> MC.string "slashing" $> Slashing
        <|> MC.string "bludgeoning" $> Bludgeoning
        <|> MC.string "radiation" $> Radiation

-- | Run one battle round with optional commentary
runOne :: State -> State
runOne s = Map.filter ((> 0) . n) s'
  where
    allGroupsByPower :: [GroupIndex] = indicesByEffPower s
    targetAssignments :: [(GroupIndex, GroupIndex)] = L.foldl' (assignTarget s) [] allGroupsByPower
    sortedAssignments :: [(GroupIndex, GroupIndex)] = L.sortBy attackerInitiative targetAssignments
    s' = L.foldl' resolveAttack s sortedAssignments
    attackerInitiative :: (GroupIndex, GroupIndex) -> (GroupIndex, GroupIndex) -> Ordering
    attackerInitiative (a1, _) (a2, _) = compare (initiative (s Map.! a2)) (initiative (s Map.! a1))

-- Select a target for the group and return the index of the selected group
assignTarget :: State -> [(GroupIndex, GroupIndex)] -> GroupIndex -> [(GroupIndex, GroupIndex)]
assignTarget s assigned i
    | null availableTargets = assigned -- no targets left to choose
    | damage == 0 = assigned -- all remaining targets are immune
    | otherwise = (i, target) : assigned
  where
    assignedTargets :: [GroupIndex] = map snd assigned
    thisSide :: Side = side (s Map.! i)
    enemies :: State = Map.filter ((/= thisSide) . side) s
    availableTargets :: [GroupIndex] = filter (`notElem` assignedTargets) $ Map.keys enemies
    availableDamages :: [(GroupIndex, Int)] = map (\t -> (t, potentialDamage (s Map.! i) (s Map.! t))) availableTargets
    (target, damage) = Fold.maximumBy compareTargets availableDamages
    compareTargets :: (GroupIndex, Int) -> (GroupIndex, Int) -> Ordering
    compareTargets (t1, d1) (t2, d2) = case compare d1 d2 of
        LT -> LT
        GT -> GT
        EQ -> case compare (effPower (s Map.! t1)) (effPower (s Map.! t2)) of
            LT -> LT
            GT -> GT
            EQ -> case compare (initiative (s Map.! t1)) (initiative (s Map.! t2)) of
                LT -> LT
                GT -> GT
                EQ -> error "Cannot split targets"

-- | Update state for give attack
resolveAttack :: State -> (GroupIndex, GroupIndex) -> State
resolveAttack s (a, d) = Map.insert d (defender{n = remaining}) s
  where
    defender = s Map.! d
    damage = potentialDamage (s Map.! a) defender
    potentialKilled = damage `div` hp defender
    killed = min (n defender) potentialKilled
    remaining = n defender - killed

{- | Return the effective power of a group
 >>> map (\g -> (n g, effPower g)) $ Map.elems exampleState
 [(17,76619),(989,24725),(801,92916),(4485,53820)]
-}
effPower :: Group -> Int
effPower u = n u * atk u

-- | Potential damage done by group attacking another (ignoring damage lost with partial units)
potentialDamage :: Group -> Group -> Int
potentialDamage gAtk gDef = typeFactor (n gAtk * atk gAtk) (atkType gAtk) (weak gDef) (immune gDef)
  where
    typeFactor :: Int -> AtkType -> [AtkType] -> [AtkType] -> Int
    typeFactor d a isWeak isImmune
        | a `elem` isWeak = d * 2
        | a `elem` isImmune = 0
        | otherwise = d

{- | Return the group indices sorted in descending order of attack power (with tiebreakers)

 >>> map (\i -> n (exampleState ! i)) $ indicesByEffPower exampleState
 [801,17,4485,989]
-}
indicesByEffPower :: State -> [GroupIndex]
indicesByEffPower s = L.sortBy (flip comparePower) $ Map.keys s
  where
    comparePower :: GroupIndex -> GroupIndex -> Ordering
    comparePower i1 i2 = case compare (effPower u1) (effPower u2) of
        EQ -> compare (initiative u1) (initiative u2)
        x -> x
      where
        u1 = s Map.! i1
        u2 = s Map.! i2

-- | Apply runOne until only one side remains or stalemate
runAll :: State -> State
runAll s
    | numSideGroups ImmuneSystem s == 0 || numSideGroups Infection s == 0 = s
    | otherwise =
        let s' = runOne s
         in if numUnits s' == numUnits s
                then s
                else runAll s'

-- | Boost until immune wins
boostUntilWin :: State -> (Int, State)
boostUntilWin s = go 0
  where
    go :: Int -> (Int, State)
    go x =
        let s' = runAll (boost x s)
         in if numSideGroups ImmuneSystem s' > 0 && numSideGroups Infection s' == 0
                then (x, s')
                else go (x + 1)

-- | Increase attack power of immune system groups
boost :: Int -> State -> State
boost x = Map.map boostGroup
  where
    boostGroup :: Group -> Group
    boostGroup g
        | side g == ImmuneSystem = g{atk = x + atk g}
        | otherwise = g

-- | number of Units in a state
numUnits :: State -> Int
numUnits = sum . map n . Map.elems

-- Number of Groups on a side
numSideGroups :: Side -> State -> Int
numSideGroups s = Map.size . Map.filter ((== s) . side)

-- exampleState :: State
-- exampleState =
--     readState
--         "\
--         \Immune System:\n\
--         \17 units each with 5390 hit points (weak to radiation, bludgeoning) \
--         \with an attack that does 4507 fire damage at initiative 2\n\
--         \989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) \
--         \with an attack that does 25 slashing damage at initiative 3\n\n\
--         \Infection:\n\
--         \801 units each with 4706 hit points (weak to radiation) \
--         \with an attack that does 116 bludgeoning damage at initiative 1\n\
--         \4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) \
--         \with an attack that does 12 slashing damage at initiative 4\n"