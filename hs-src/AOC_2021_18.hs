{- |
 Module      : AOC_2021_18
 Description : Advent of code 2021 day 18
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2021_18 (solvers, add, addList, magnitude, readSnail, reduce, showSnail, SnailNumber) where

import Data.Char qualified as C (isDigit)
import Data.List qualified as L (foldl1')
import Data.Text (Text)
import Data.Text qualified as T (lines, pack, unpack)
import Debug.Trace (trace)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show . magnitude $ addList numbers
    , "NYI" -- T.pack . show $ eval packet
    )
  where
    numbers = map readSnail $ T.lines t

data SnailTerm = Open | Close | Comma | Reg Int

type SnailNumber = [SnailTerm]

addList :: [SnailNumber] -> SnailNumber
addList = L.foldl1' add

add :: SnailNumber -> SnailNumber -> SnailNumber
add x y = trace ("  " ++ showSnail x ++ "\n+ " ++ showSnail y ++ "\n= " ++ showSnail z ++ "\n") z
  where
    z = reduce $ [Open] ++ x ++ [Comma] ++ y ++ [Close]

--add x y = reduce $ [Open] ++ x ++ [Comma] ++ y ++ [Close]

-- Apply explosions and split until no more possible
reduce :: SnailNumber -> SnailNumber
reduce xs = trace (showSnail xs) $ maybe xs reduce $ reduce' 0 [] xs

-- reduce xs = maybe xs reduce $ reduce' 0 [] xs

-- Apply first explosion or split to the list. Nothing if no reduction to make
reduce' :: Int -> SnailNumber -> SnailNumber -> Maybe SnailNumber
-- Explode a pair
reduce' n left right = case (n, right) of
    (4, Open : Reg x : Comma : Reg y : Close : rest) -> Just $ addToLast x left ++ [Reg 0] ++ addToFirst y rest
    (_, Reg x : rest) ->
        if x >= 10
            then Just $ left ++ [Open, Reg (x `div` 2), Comma, Reg (x `div` 2 + if odd x then 1 else 0), Close] ++ rest
            else reduce' n (left ++ [Reg x]) rest
    (_, Open : rest) -> reduce' (n + 1) (left ++ [Open]) rest
    (_, Close : rest) -> reduce' (n - 1) (left ++ [Close]) rest
    (_, Comma : rest) -> reduce' n (left ++ [Comma]) rest
    (_, []) -> Nothing

{-

[[[[[1,1],[2,2]],[3,3]],[4,4]],[5,5]]
[[[[0,[3,2]],[3,3]],[4,4]],[5,5]]
[[[[3,0],[5,3]],[4,4]],[5,5]]

[[[[4,0],[5,4]],[[7,7],[6,0]]],[[[6,6],[5,6]],[[6,0],[7,7]]]]
0123   3 3   32 23   3 3   321 123   3 3   32 23   3 3   3210

-}

-- reduceIfPossible :: Int -> SnailNumber -> SnailNumber
-- reduceIfPossible n x = case reduce' n [] x of
--     Just y -> y
--     Nothing -> x

-- Add z to the first regular term in the sequence
addToFirst :: Int -> SnailNumber -> SnailNumber
addToFirst z (Reg x : rest) = Reg (x + z) : rest
addToFirst z (y : rest) = y : addToFirst z rest
addToFirst _ [] = []

addToLast :: Int -> SnailNumber -> SnailNumber
addToLast z = reverse . addToFirst z . reverse

magnitude :: SnailNumber -> Int
magnitude = const 0

-- magnitude (SnailReg x) = x
-- magnitude (SnailPair _ p q) = 3 * magnitude p + 2 * magnitude q

readSnail :: Text -> SnailNumber
readSnail = go . T.unpack
  where
    go ('[' : xs) = Open : go xs
    go (']' : xs) = Close : go xs
    go (',' : xs) = Comma : go xs
    go (x : xs)
        | C.isDigit x =
            let (num, rest) = span C.isDigit (x : xs)
             in Reg (read num) : go rest
        | otherwise = error "Unexpected character"
    go [] = []

showSnail :: SnailNumber -> String
showSnail = concatMap showTerm
  where
    showTerm :: SnailTerm -> String
    showTerm Open = "["
    showTerm Close = "]"
    showTerm Comma = ","
    showTerm (Reg x) = show x
