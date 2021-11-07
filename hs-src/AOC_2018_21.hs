{- |
 Module      : AOC_2018_21
 Description : Advent of code 2018 day 21
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental


Examining our program we can see that the program halts (from line 28 if r5 matches r0).

For part (a) we need the smallest integer for r0 that causes the program to terminate
in the shortest possible time, we just run until line 28 and check the value of r5
which is then the value of r0 we need.

For part (b) we need the last value of r5 before it starts to loop

cspell:disable

#ip 4
# Preamble - infinite loop if our and instruction does not work
0   seti 123 0 5        r5 = 123
1L  bani 5 456 5        r5 &= 456
2   eqri 5 72 5         if r5 /= 72 then jump to instruction 1
3   addr 5 4 4          ..
4   seti 0 0 4          ..
# Main program
5   seti 0 6 5          r5 = 0
6L  bori 5 65536 1      r1 = r5 | x01_00_00     r1 is r5 with bit 4 set
7   seti 4591209 6 5    r5 = x46_0E_69
8L  bani 1 255 3        r3 = r1 & xFF           r3 is low byte of r1 (=old r5)
9   addr 5 3 5          r5 += r3
10  bani 5 16777215 5   r5 &= xFF_FF_FF
11  muli 5 65899 5      r5 *= x01_01_6B
12  bani 5 16777215 5   r5 &= xFF_FF_FF
13  gtir 256 1 3        if 256 > r1 then jump to instruction 28
14  addr 3 4 4          ..
15  addi 4 1 4          ..
16  seti 27 7 4         ..
17  seti 0 0 3          r3 = 0
18L addi 3 1 2          r2 = r3 + 1
19  muli 2 256 2        r2 *= 256
20  gtrr 2 1 2          if r2 > r1 then jump to instruction 26
21  addr 2 4 4          ..
22  addi 4 1 4          ..
23  seti 25 4 4         ..
24  addi 3 1 3          r3 += 1
25  seti 17 0 4         Jump to instruction 18
26L setr 3 4 1          r1 = r3
27  seti 7 2 4          Jump to instruction 8
28L eqrr 5 0 3          if r5 == r0 then HALT
29  addr 3 4 4          ..
30  seti 5 1 4          Jump to instruction 6
cspell:disable
-}
module AOC_2018_21 (solvers) where

import Data.Array.IArray qualified as A ((!))
import Data.Set qualified as Set (member, insert, empty)
import Data.Text (Text)
import Data.Text qualified as T (lines, pack)

import AOC_2018_19 (State (..), initialState, readIP, readProgram, step, runUntil, Program)

solvers :: Text -> (Text, Text)
solvers t = case T.lines t of
    [] -> error "No input"
    (ipStr : progStr) ->
        ( T.pack $ show a
        , T.pack . show . lastBeforeRepeat $ repeatRunUntil 28 prog start
        )
      where
        prog = readProgram progStr
        start = initialState (readIP ipStr)
        a :: Int = (A.! 5) . registers $ runUntil 28 prog start

-- return a list of r5 values when stopping at given instruction
repeatRunUntil :: Int -> Program -> State -> [Int]
repeatRunUntil ins prog s = r5 : repeatRunUntil ins prog s''
  where
    s' = runUntil ins prog s
    s'' = step s' prog
    r5 = (A.! 5) $ registers s'

-- Return last elemet of a list before any repeat
lastBeforeRepeat :: [Int] -> Int
lastBeforeRepeat = go Set.empty 0
    where
        go _ _ [] = error "Unexpected end of list"
        go s prev (x : xs)
            | x `Set.member` s = prev
            | otherwise = go (Set.insert x s) x xs
