{- |
 Module      : AOC_2018_16
 Description : Advent of code 2018 day 16
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_16 (solvers) where

import Data.Array.IArray (Array)
import Data.Array.IArray qualified as A (listArray, (!), (//))
import Data.Bits (Bits ((.&.), (.|.)))
import Data.List qualified as L (foldl')
import Data.Map (Map)
import Data.Map qualified as Map (empty, filter, findMin, insert, lookup, map, size, (!))
import Data.Maybe qualified as Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T (breakOn, drop, lines, pack, snoc, stripPrefix, unpack, words)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ length (filter ((>= 3) . length) (map possibleOps samples))
    , T.pack . show $ output A.! 0
    )
  where
    (inputA, rest) = T.breakOn "\n\n\n\n" t
    samples = map readSample . bunch . T.lines $ (inputA `T.snoc` '\n') `T.snoc` '\n'
    opcodeAssignments = assignOpcodes samples
    opcodeDeductions = deduceOpcodes opcodeAssignments
    inputB = T.drop 4 rest
    program = map (readInstruction . T.unpack) $ T.lines inputB
    output = run opcodeDeductions program

type Instruction = Array Int Int

type Registers = Array Int Int

-- cspell:disable

data Operation
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
    deriving (Enum, Eq, Show)

-- cspell:enable

allOperations :: [Operation]
allOperations = [Addr .. Eqrr]

-- Result of applying the given opcode with the interpreted as the given operation
applyAs :: Operation -> Instruction -> Registers -> Registers
applyAs op i r = r A.// [(c, newVal)]
  where
    (a, b, c) = (i A.! 1, i A.! 2, i A.! 3)
    newVal = case op of
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

{- | Which Operations are possible interpretations of the instruction and before/after registers

 >>> possibleOps ((ar [3,2,1,1]),(ar [9,2,1,2]),(ar [3,2,2,1]))
 [Addi,Mulr,Seti]
-}

-- cspell:enable

possibleOps :: (Registers, Instruction, Registers) -> [Operation]
possibleOps (r, i, r') = filter possible allOperations
  where
    possible :: Operation -> Bool
    possible op = applyAs op i r == r'

-- Assign possible operations to each opcode
assignOpcodes :: [(Registers, Instruction, Registers)] -> Map Int [Operation]
assignOpcodes = L.foldl' assign Map.empty
  where
    assign :: Map Int [Operation] -> (Registers, Instruction, Registers) -> Map Int [Operation]
    assign m (r, i, r') = case Map.lookup code m of
        Just p -> Map.insert code (filter (`elem` p') p) m
        Nothing -> Map.insert code p' m
      where
        code = i A.! 0
        p' = possibleOps (r, i, r')

-- Allocate opcodes to each instruction
deduceOpcodes :: Map Int [Operation] -> Map Int Operation
deduceOpcodes mp = snd $ go (mp, Map.empty)
  where
    go :: (Map Int [Operation], Map Int Operation) -> (Map Int [Operation], Map Int Operation)
    go (m, assigned)
        | Map.size assigned == Map.size m = (m, assigned)
        | otherwise = go (m', assigned')
      where
        -- First code that is unambiguous
        (code, instructions) :: (Int, [Operation]) = Map.findMin $ Map.filter ((== 1) . length) m
        instruction = case instructions of
            [ins] -> ins
            _ -> error "Expected only one instruction"
        m' = Map.map (filter (/= instruction)) m
        assigned' = Map.insert code instruction assigned

-- run the program
run :: Map Int Operation -> [Instruction] -> Registers
run m = L.foldl' f zeros
  where
    zeros = A.listArray (0, 3) [0, 0, 0, 0]
    f :: Registers -> Instruction -> Registers
    f r i = applyAs (m Map.! (i A.! 0)) i r

{- | Read a sample from 4 strings

 >>> readSample ("Before: [1, 0, 2, 1]", "2 3 2 0", "After:  [1, 0, 2, 4]", "")
 (array (0,3) [(0,1),(1,0),(2,2),(3,1)],array (0,3) [(0,2),(1,3),(2,2),(3,0)],array (0,3) [(0,1),(1,0),(2,2),(3,4)])
-}
readSample :: (Text, Text, Text, Text) -> (Registers, Instruction, Registers)
readSample (a, b, c, _) = (A.listArray (0, 3) a', A.listArray (0, 3) b', A.listArray (0, 3) c')
  where
    a' :: [Int] = read . T.unpack . Maybe.fromJust $ T.stripPrefix "Before: " a
    b' :: [Int] = map (read . T.unpack) $ T.words b
    c' :: [Int] = read . T.unpack . Maybe.fromJust $ T.stripPrefix "After:  " c

{- | Read an instruction

 >> readInstruction "10 0 1 3"
 array (0,3) [(0,10),(1,0),(2,1),(3,1)]
-}
readInstruction :: String -> Instruction
readInstruction = A.listArray (0, 3) . map read . words

-- Bunch a list into a list of 4-tuples
bunch :: [x] -> [(x, x, x, x)]
bunch [] = []
bunch (a : b : c : d : rest) = (a, b, c, d) : bunch rest
bunch _ = error "List length not a multiple of 4 in bunch"
