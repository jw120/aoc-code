{- |
 Module      : AOC_2021_08
 Description : Advent of code 2021 day 8
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental

Haskell solution trying to avoid any hand-calculations and derive all the logic from
the digitSegments mapping.
-}
module AOC_2021_08 (solvers) where

import Data.Text (Text)

solvers :: Text -> (Text, Text)
solvers = undefined

{-

import Data.List qualified as L (foldl')
import Data.Map (Map)
import Data.Map qualified as Map (elems, empty, filter, fromList, insertWith, keys, toList, (!))
import Data.Maybe qualified as Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set (fromList)
import Data.Text (Text)
import Data.Text qualified as T (lines, pack, splitOn, unpack, words)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show . count1478 $ concatMap snd problems
    , "NYI" -- T.pack . show . sum $ map solve problems
    )
  where
    problems :: [([String], [String])] = map parseLine $ T.lines t
    parseLine :: Text -> ([String], [String])
    parseLine u = case T.splitOn " | " u of
        [signals, outputs] -> (splitPart signals, splitPart outputs)
        _ -> error "Unexpected line format"
    splitPart :: Text -> [String]
    splitPart = map T.unpack . T.words

-- cspell: disable
digitSegments :: Map Int String
digitSegments =
    Map.fromList
        [ (0, "abcefg")
        , (1, "cf")
        , (2, "acdeg")
        , (3, "acdfg")
        , (4, "bcdf")
        , (5, "abdfg")
        , (6, "abdefg")
        , (7, "acf")
        , (8, "abcdefg")
        , (9, "abcdfg")
        ]

allSegments :: String
allSegments = "abcdefg"

-- | Number of outputs that are 1478 (which have unique lengths)
count1478 :: [String] -> Int
count1478 = length . filter ((`elem` lengths1478) . length)
  where
    lengths1478 = map (length . (digitSegments Map.!)) [1, 4, 7, 8]

type Digit = Int
type Wire = Char
type Segment = Char
type Assignment = Map Wire (Set Segment)

solve :: ([String], [String]) -> Int
solve (signals, outputs) = combineDigits $ map (applyAssignment assignments) outputs
  where
    assignments :: Assignment = L.foldl' assignWires allAllowed $ lengthConstraints signals ++ countConstraints signals

allAllowed :: Assignment
allAllowed = Map.fromList $ map (,Set.fromList allSegments) allSegments

lengthConstraints :: [[Wire]] -> [(Set Wire, Set Segment)]
lengthConstraints = undefined

countConstraints :: [[Wire]] -> [(Set Wire, Set Segment)]
countConstraints signals = Maybe.mapMaybe () $ Map.toList wireCounts
  where
    wireCounts :: Count Wire = count $ concat signals
    -- How many times does each segment appear (in the digits 0..9)
    segment09Counts :: Count Segment = count . concat $ Map.elems digitSegments
    -- Which of these segment counts are unique
    uniqueSegment09Counts :: [Int] = Map.keys . Map.filter (== 1) . count $ Map.elems segment09Counts
    -- Mapping from a segment count to the segment
    segment09CountToSegment :: Int -> Maybe Segment
    segment09CountToSegment count
        | count `elem` uniqueSegment09Counts = Just $ invert segment09Counts Map.! count
        | otherwise = Nothing

-- | Update tassignment: given wires must be given segments, other wires cannot be those segments
assignWires :: Assignment -> (Set Wire, Set Segment) -> Assignment
assignWires = undefined

applyAssignment :: Assignment -> [Wire] -> Int
applyAssignment = undefined

combineDigits :: [Int] -> Int
combineDigits [a, b, c, d] = 1000 * a + 100 * b + 10 * c + d
combineDigits _ = error "Wrong number of digits"

type Count a = Map a Int

count :: Ord a => [a] -> Count a
count = L.foldl' (\m x -> Map.insertWith (+) x 1 m) Map.empty

invert :: Ord a => Map k a -> Map a k
invert = Map.fromList . map (\(x, y) -> (y, x)) . Map.toList

-}

{-
"""Advent of Code 2021 - Day 8."""

from collections import Counter
from doctest import testmod
from sys import stdin
from typing import Dict, Iterator, Set

# cspell:disable
test1: str = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
test2: list[str] = [
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
    "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
    "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
    "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
    "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
    "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
    "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
    "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
    "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce",
]

# Mapping from digits to number of segments:
#
# Digit     Number of segments
# 0             6
# 1             2 (only digit with 2 segments)
# 2             5
# 3             5
# 4             4 (only digit with 4 segments)
# 5             5
# 6             6
# 7             3 (only digit with 3 segments)
# 8             7 (only digit with 7 segments)
# 9             6
#
# Mapping from number of segments to digits
# 2 -> 1
# 3 -> 7
# 4 -> 4
# 5 -> 2/3/5
# 6 -> 0/6/9
# 7 -> 8

# Mapping from segments to digits in which the are present
# seg_dig = {'a': set([0, 2, 3, 5, 6, 7, 8, 9]), 'b': set([0, 4])

segments_to_digit: Dict[str, int] = {
    "abcefg": 0,
    "cf": 1,
    "acdeg": 2,
    "acdfg": 3,
    "bcdf": 4,
    "abdfg": 5,
    "abdefg": 6,
    "acf": 7,
    "abcdefg": 8,
    "abcdfg": 9,
}

def num_output_1478(lines: list[str]) -> int:
    """Count number of times digits 1478 appear in outputs.

    >>> num_output_1478(test2)
    26
    """
    return sum(
        len(word) in [2, 3, 4, 7]
        for line in lines
        for word in line.split(" | ")[1].split()
    )

def solve(line: str) -> int:
    """Return the 4-digit output from the given set of inputs.

    >>> solve(test1)
    5353
    >>> sum(solve(line) for line in test2)
    61229
    """
    signals, outputs = (part.split() for part in line.split(" | "))

    # To start with every wire could be any segment
    wire_to_segment: Dict[str, Set[str]] = {x: set("abcdefg") for x in "abcdefg"}

    def match_wires(signal: str, segments: str) -> None:
        """Match the wires in the signal to the given segments.

        Any wire in the signal must be one of the segments. Any wire not in the signal
        cannot be one of these segments.
        """
        for wire in "abcdefg":
            if wire in signal:
                wire_to_segment[wire] &= set(segments)
            else:
                wire_to_segment[wire] -= set(segments)

    for signal in signals:
        if len(signal) == 2:  # Must be a '1' which has segments c & f
            match_wires(signal, "cf")
        elif len(signal) == 3:  # Must be a '7' which has segments a, c & f
            match_wires(signal, "acf")
        elif len(signal) == 4:  # Must be a '4' which has segments b, c, d, f
            match_wires(signal, "bcdf")

    wire_counts = Counter("".join(signals))
    for wire in "abcdefg":
        if wire_counts[wire] == 4:  # Must be 'e' segment which only appears in 4 digits
            match_wires(wire, "e")
        elif wire_counts[wire] == 6:
            match_wires(wire, "b")
        elif wire_counts[wire] == 9:
            match_wires(wire, "f")

    return digits_to_int(
        segments_to_digit[
            "".join(
                sorted(segment for wire in output for segment in wire_to_segment[wire])
            )
        ]
        for output in outputs
    )

def digits_to_int(xs: Iterator[int]) -> int:
    a, b, c, d = xs
    return 1000 * a + 100 * b + 10 * c + d

if __name__ == "__main__":
    testmod()
    lines: list[str] = stdin.read().splitlines()
    print(num_output_1478(lines))
    print(sum(solve(line) for line in lines))

-}