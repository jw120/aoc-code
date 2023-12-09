{- |
 Module      : AOC_2018_02
 Description : Advent of code 2018 day 2
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_02 (solvers) where

import Data.List qualified as L (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T (pack, unpack)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ checkSum ls
    , T.pack $ pairWithOneDiff ls
    )
  where
    ls = lines $ T.unpack t

-- Part (a) solution. Product of number of ids with a 2-count character and the number
-- with a 3-count character
checkSum :: [String] -> Int
checkSum ids = count2 * count3
  where
    (count2, count3) = L.foldl' accPair (0, 0) $ map hasReps ids
    accPair :: (Int, Int) -> (Bool, Bool) -> (Int, Int)
    accPair (acc1, acc2) (x1, x2) =
        (acc1 + if x1 then 1 else 0, acc2 + if x2 then 1 else 0)

-- | Does the list contain a doubly-repeated and/or a triply-repeated element
hasReps :: Ord k => [k] -> (Bool, Bool)
hasReps s = (2 `elem` counts, 3 `elem` counts)
  where
    counts = Map.elems $ buildCountMap s

-- | Build a map of the counts of each element of the list
buildCountMap :: Ord k => [k] -> Map k Int
buildCountMap = L.foldl' addCount Map.empty
  where
    addCount m x = Map.insertWith (+) x 1 m

-- Part (b) solution. Finds pair of ids which have only one differing character and
-- return the common characters
pairWithOneDiff :: [String] -> String
pairWithOneDiff ids = commonElements
  where
    commonElements = map fst . filter (uncurry (==)) $ zip id1 id2
    (_, id1, id2) = head $ filter (\(d, _, _) -> d) labelledPairs
    labelledPairs = map (\(id_1, id_2) -> (hasOneDiff id_1 id_2, id_1, id_2)) pairs
    pairs = allPairs ids

-- | Generate all unordered pairs of elements from a list
allPairs :: [a] -> [(a, a)]
allPairs (x : xs) = zip (repeat x) xs ++ allPairs xs
allPairs [] = []

-- | Are the two lists identical except for precisely one difference
hasOneDiff :: Eq a => [a] -> [a] -> Bool
hasOneDiff = hasOneDiff' False
  where
    hasOneDiff' found (x : xs) (y : ys)
        | x == y = hasOneDiff' found xs ys
        | -- Keep going if not different
          found =
            False
        | -- Fail if find second difference
          otherwise =
            hasOneDiff' True xs ys -- Keep going with first difference
    hasOneDiff' found [] [] = found -- Succeed if only one difference and both at end
    hasOneDiff' _ _ _ = False -- Fail if mismatched lists
