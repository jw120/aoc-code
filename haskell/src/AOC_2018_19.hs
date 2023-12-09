{- |
 Module      : AOC_2018_19
 Description : Advent of code 2018 day 19
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental

Day 19 involves running a program on the device that (disassembly show) returns the sum of factors of a large integer.
The part B version takes an unfeasible length of time, so we have to shortcut its solution.

Program below
    - First jumps to instruction 17 and sets r5 to the target value
    - Then jumps nack to instruction 1 and accumulates in r0 all the factors of r5 by cycling over r1 and r2

cspell:disable
#ip 4
0   addi 4 16 4     goto 17L                   jmp +16
1L  seti 1 5 1      r1 = 1
2L  seti 1 2 2      r2 = 2
3L   mulr 1 2 3     r3 = r1 * r2
4   eqrr 3 5 3      if r3 == r5: r0+=r1        r3 = r3 == r5
5   addr 3 4 4
6   addi 4 1 4
7   addr 1 0 0
8   addi 2 1 2      r2 += 1
9   gtrr 2 5 3      if r2 <= r5: jmp 3L        r3 = r2 > r5
10  addr 4 3 4
11  seti 2 7 4
12  addi 1 1 1      r1 += 1
13  gtrr 1 5 3      if r1 <= r5: jmp 2L
14  addr 3 4 4
15  seti 1 9 4
16  mulr 4 4 4      halt                        ip = 256
17L addi 5 2 5                                  r5 += 2
18  mulr 5 5 5                                  r5 = r5 * r5
19  mulr 4 5 5                                  r5 = r4 * r5
20  muli 5 11 5     r5 = 11 * r4*(r5 + 2)^2     r5 = r5 * 11
21  addi 3 1 3                                  r3++
22  mulr 3 4 3                                  r3=r3*r4
23  addi 3 18 3     r3 = (r3 + 1)*r4 + 18       r3 += 18
24  addr 5 3 5      r5 = r3 + r5                r5 = r3 + r5
25  addr 4 0 4      ip += r0
26  seti 0 3 4      r4 = 0                      r4 = 0
27  setr 4 2 3      r3 = r4                     r3 = r4
28  mulr 3 4 3      r3 *= r4
29  addr 4 3 3      r3 += r4
30  mulr 4 3 3      r3 *= r4
31  muli 3 14 3     r3 *= 14
32  mulr 3 4 3      r3 *= r4
33  addr 5 3 5      r5 += r3
34  seti 0 4 0      r0 = 0
35  seti 0 5 4      jump L1
cspell:disable
-}
module AOC_2018_19 (solvers, initialState, readIP, readProgram, runUntil, step, Program, State (..)) where

import Data.Array.IArray (Array)
import Data.Array.IArray qualified as A (elems, listArray, (!), (//))
import Data.Bits (Bits ((.&.), (.|.)))
import Data.Text (Text)
import Data.Text qualified as T (lines, pack, toTitle, unpack, words)
import Utilities (readUnsignedDecimal)

solvers :: Text -> (Text, Text)
solvers t = case T.lines t of
    [] -> error "No input"
    (ipStr : progStr) ->
        ( T.pack . show $ sumFactors a
        , T.pack . show $ sumFactors b
        )
      where
        prog :: Program = readProgram progStr
        aStart :: State = initialState (readIP ipStr)
        bStart :: State = aStart{registers = registers aStart A.// [(0, 1)]}
        a :: Int = (A.! 5) . registers $ runUntil 1 prog aStart
        b :: Int = (A.! 5) . registers $ runUntil 1 prog bStart
        sumFactors :: Int -> Int
        sumFactors x = sum [i | i <- [1 .. x], x `mod` i == 0]

type Registers = Array Int Int -- always size 6

data State = State
    { ipBinding :: Int
    , ipValue :: Int
    , registers :: Registers
    }

instance Show State where
    show s = show (ipValue s) ++ ":" ++ show (A.elems (registers s))

initialState :: Int -> State
initialState ipAssignment =
    State
        { ipBinding = ipAssignment
        , ipValue = 0
        , registers = A.listArray (0, 5) [0, 0, 0, 0, 0, 0]
        }

-- cspell:disable

data OpCode
    = Addr
    | Addi
    | Mulr
    | Muli
    | Seti
    | Setr
    | Banr
    | Bani
    | Borr
    | Bori
    | Gtir
    | Gtri
    | Gtrr
    | Eqir
    | Eqri
    | Eqrr
    deriving (Enum, Eq, Read, Show)

-- cspell:enable

data Instruction = Instruction
    { opCode :: OpCode
    , insA :: Int
    , insB :: Int
    , insC :: Int
    }

instance Show Instruction where
    show i =
        unwords
            [ show (opCode i)
            , show (insA i)
            , show (insB i)
            , show (insC i)
            ]

type Program = Array Int Instruction

-- | Read an instruction
readInstruction :: Text -> Instruction
readInstruction t = case T.words t of
    [opString, aString, bString, cString] ->
        Instruction
            { opCode = read . T.unpack $ T.toTitle opString
            , insA = readUnsignedDecimal aString
            , insB = readUnsignedDecimal bString
            , insC = readUnsignedDecimal cString
            }
      where
    _ -> error "Invalid instruction"

readProgram :: [Text] -> Program
readProgram ts = A.listArray (0, length ts - 1) $ map readInstruction ts

-- | Read the IP assignment line
readIP :: Text -> Int
readIP t = case T.words t of
    ["#ip", ipText] -> readUnsignedDecimal ipText
    _ -> error $ "Invalid ip line" ++ T.unpack t

-- | Step one instruction of the program
step :: State -> Program -> State
step s p = s{registers = r', ipValue = ipValue'}
  where
    instruction = p A.! ipValue s
    r = registers s A.// [(ipBinding s, ipValue s)]
    r' = r A.// [(c, newVal)]
    ipValue' = (r' A.! ipBinding s) + 1
    (a, b, c) = (insA instruction, insB instruction, insC instruction)
    newVal = case opCode instruction of
        -- cspell:disable
        Addr -> r A.! a + r A.! b
        Addi -> r A.! a + b
        Mulr -> r A.! a * r A.! b
        Muli -> r A.! a * b
        Banr -> r A.! a .&. r A.! b
        Bani -> r A.! a .&. b
        Borr -> r A.! a .|. r A.! b
        Bori -> r A.! a .|. b
        Gtir -> if a > r A.! b then 1 else 0
        Gtri -> if r A.! a > b then 1 else 0
        Gtrr -> if r A.! a > r A.! b then 1 else 0
        Eqir -> if a == r A.! b then 1 else 0
        Eqri -> if r A.! a == b then 1 else 0
        Eqrr -> if r A.! a == r A.! b then 1 else 0
        Setr -> r A.! a
        Seti -> a

-- cspell:enable

-- | Run the program until it halts or reaches the given IP value
runUntil :: Int -> Program -> State -> State
runUntil x p = go
  where
    go :: State -> State
    go s
        | ipValue s >= 0 && ipValue s < length p && ipValue s /= x = go (step s p)
        | otherwise = s

-- showSome :: Int -> Program -> State -> Text
-- showSome n program state = T.unlines . map (T.pack . show) . take n . zip [0 :: Int ..] $ iterate (`step` program) state
