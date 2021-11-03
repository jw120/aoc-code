{- |
 Module      : AOC_2018_19
 Description : Advent of code 2018 day 19
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_19 (solvers) where

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
        ( T.pack . show $ registers finalState A.! 0
        , "NYI" -- T.pack . show $ registers finalState' A.! 0
        )
      where
        startState :: State = initialState (readIP ipStr)
        prog :: Program = readProgram progStr
        finalState :: State = run prog startState
        startState' = startState{registers = registers startState A.// [(0, 1)]}
        finalState' :: State = run prog startState'

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

{- | Read an instruction

 >>> readInstruction "seti 6 0 2"
 Seti 6 0 2
-}
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

{- | Read the IP assignment line

 >>> readIP "#ip 4"
 4
-}
readIP :: Text -> Int
readIP t = case T.words t of
    ["#ip", ipText] -> readUnsignedDecimal ipText
    _ -> error $ "Invalid ip line" ++ T.unpack t

{- | Step one instruction of the program

 >>> step (initialState 0) testProgram
 1:[0,5,0,0,0,0]
 >>> step (step (initialState 0) testProgram) testProgram
 2:[1,5,6,0,0,0]
 >>> step (step (step (initialState 0) testProgram) testProgram) testProgram
 4:[3,5,6,0,0,0]
 >>> step (step (step (step (initialState 0) testProgram) testProgram) testProgram) testProgram
 6:[5,5,6,0,0,0]
 >>> step (step (step (step (step (initialState 0) testProgram) testProgram) testProgram) testProgram) testProgram
 7:[6,5,6,0,0,9]
-}
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

{- | Run the program until it halts

 >>> run testProgram (initialState 0)
 7:[6,5,6,0,0,9]
-}
run :: Program -> State -> State
run p = go
  where
    go :: State -> State
    go s
        | ipValue s >= 0 && ipValue s < length p = go (step s p)
        | otherwise = s
