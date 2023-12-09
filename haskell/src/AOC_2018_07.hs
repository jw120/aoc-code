{- |
 Module      : AOC_2018_07
 Description : Advent of code 2018 day 7
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_07 (solvers) where

import Data.Char qualified as C (ord, toLower)
import Data.List qualified as L (foldl', nub, sort)
import Data.Map (Map)
import Data.Map qualified as Map (fromList, insertWith, keys, (!))
import Data.Text (Text)
import Data.Text qualified as T (lines, pack)
import Text.Megaparsec.Char qualified as MC (asciiChar, string)

import Utilities (Parser, parseOrStop)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack $ allSteps rules
    , T.pack . show $ allParallelSteps rules 5 60
    )
  where
    rules = map (parseOrStop pRule) $ T.lines t

pRule :: Parser (Char, Char)
pRule = do
    x <- MC.string "Step " *> MC.asciiChar
    y <- MC.string " must be finished before step " *> MC.asciiChar
    _ <- MC.string " can begin."
    return (x, y)

-- | Convert a list of rules into a map from chars to their direct prerequisites
toPrerequisiteMap :: [(Char, Char)] -> Map Char String
toPrerequisiteMap rules = L.foldl' addRule allEmpty rules
  where
    addRule :: Map Char String -> (Char, Char) -> Map Char String
    addRule m (prerequisite, step) = Map.insertWith addSorted step [prerequisite] m
    addSorted :: String -> String -> String
    addSorted s1 s2 = L.sort (s1 ++ s2)
    allEmpty :: Map Char String
    allEmpty = Map.fromList . map (,"") $ L.nub (map fst rules ++ map snd rules)

-- | What is the next step to be performed for given prerequisite map and already completed steps
nextStep :: Map Char String -> String -> Maybe Char
nextStep prerequisites done = case available of
    (c : _) -> Just c
    [] -> Nothing
  where
    incompleteSteps :: String = filter (`notElem` done) $ Map.keys prerequisites
    available :: String = filter prerequisitesDone incompleteSteps
    prerequisitesDone :: Char -> Bool
    prerequisitesDone step = all (`elem` done) $ prerequisites Map.! step

-- | What steps should be performed for given prerequisite map
allSteps :: [(Char, Char)] -> String
allSteps rules = allSteps' ""
  where
    prerequisiteMap :: Map Char String = toPrerequisiteMap rules
    allSteps' :: String -> String
    allSteps' done = case nextStep prerequisiteMap done of
        Just c -> allSteps' (done ++ [c])
        Nothing -> done

data ParallelState
    = Working [(Char, Int)] Int String
    | Finished
    deriving (Show)

-- | Advance one tick of parallel working
parallelStep :: Int -> Int -> Map Char String -> ParallelState -> ParallelState
parallelStep _ _ _ Finished = error "Already finished"
parallelStep n baseTime prerequisites (Working working tick done)
    | null nowIncomplete = Finished
    | otherwise = Working newWorking (tick + 1) nowDone
  where
    -- First finish the workers due to end this tick
    nowDone :: String = (done ++) . map fst $ filter ((== tick) . snd) working
    nowIncomplete :: String = filter (`notElem` nowDone) $ Map.keys prerequisites
    nowWorking :: [(Char, Int)] = filter ((> tick) . snd) working
    -- Now start new workers
    nowAvailable :: String = L.sort . filter prerequisitesDone $ filter notInProgress nowIncomplete
    newStarts :: String = take (n - length nowWorking) nowAvailable
    newWorking :: [(Char, Int)] = nowWorking ++ map (addFinishTick tick) newStarts
    -- Helpers
    prerequisitesDone :: Char -> Bool
    prerequisitesDone step = all (`elem` nowDone) $ prerequisites Map.! step
    addFinishTick :: Int -> Char -> (Char, Int)
    addFinishTick t c = (c, C.ord (C.toLower c) - C.ord 'a' + baseTime + 1 + t)
    notInProgress :: Char -> Bool
    notInProgress = (`notElem` map fst nowWorking)

{- | Run rules in parallel to completion

 >>> allParallelSteps testRules 2 0
 15
-}
allParallelSteps :: [(Char, Char)] -> Int -> Int -> Int
allParallelSteps rules n baseTime = go (Working [] 0 "")
  where
    prerequisiteMap :: Map Char String = toPrerequisiteMap rules
    go :: ParallelState -> Int
    go s@(Working _ tick _) = case parallelStep n baseTime prerequisiteMap s of
        s'@Working{} -> go s'
        Finished -> tick
    go Finished = error "Unexpected Finished state in allParallelSteps"
