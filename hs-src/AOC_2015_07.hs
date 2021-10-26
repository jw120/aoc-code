{- |
 Module      : AOC_2015_07
 Description : Advent of code 2015 day 7
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_07 (solvers, booklet, value) where

import Data.Bits qualified as Bits (complement, shift, (.&.), (.|.))
import Data.List qualified as L (foldl')
import Data.Map (Map)
import Data.Map qualified as Map (empty, insert, (!))
import Data.Text (Text)
import Data.Text qualified as T (lines, pack)
import Data.Word (Word16)
import Text.Megaparsec qualified as M (some, try)
import Text.Megaparsec.Char qualified as MC (lowerChar)

import Utilities (lexeme, pSymbol, pUnsignedInt, parseOrStop, ($>), (<|>))

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack $ show a
    , T.pack . show . snd $ partB ls a
    )
  where
    ls = T.lines t
    a = snd $ partA ls

partA :: [Text] -> (Booklet, Word16)
partA = (`value` "a") . booklet

partB :: [Text] -> Word16 -> (Booklet, Word16)
partB ls aValue = value bBooklet "a"
  where
    freshBooklet = booklet ls
    bBooklet = Map.insert "b" (Value aValue) freshBooklet

type Wire = Text

type Booklet = Map Wire Connection

data Connection
    = Value Word16
    | And Wire Wire
    | AndLit Word16 Wire -- AND gate with a literal input
    | Or Wire Wire
    | LShift Wire Int
    | RShift Wire Int
    | Not Wire
    | Direct Wire

booklet :: [Text] -> Booklet
booklet = L.foldl' (\m (c, w) -> Map.insert w c m) Map.empty . map instruction

instruction :: Text -> (Connection, Wire)
instruction = parseOrStop pInstruction
  where
    pInstruction =
        M.try pDirect
            <|> M.try pBinary
            <|> M.try pAndLit
            <|> M.try pShift
            <|> M.try pUnary
            <|> pValue
    pWire = lexeme (T.pack <$> M.some MC.lowerChar)
    pWord16 = fromIntegral <$> pUnsignedInt
    pOutput = pSymbol "->" *> pWire
    pValue = (,) <$> (Value <$> pWord16) <*> pOutput
    pDirect = (,) <$> (Direct <$> pWire) <*> pOutput
    pUnary = (,) <$> (Not <$> (pSymbol "NOT" *> pWire)) <*> pOutput
    pBinary = do
        input1 <- pWire
        isAnd <- (pSymbol "AND" $> True) <|> (pSymbol "OR" $> False)
        input2 <- pWire
        output <- pOutput
        return ((if isAnd then And else Or) input1 input2, output)
    pAndLit = do
        input1 <- pWord16
        input2 <- pSymbol "AND" *> pWire
        output <- pOutput
        return (AndLit input1 input2, output)
    pShift = do
        input1 <- pWire
        isLeft <- (pSymbol "LSHIFT" $> True) <|> (pSymbol "RSHIFT" $> False)
        input2 <- pUnsignedInt
        output <- pOutput
        return ((if isLeft then LShift else RShift) input1 input2, output)

value :: Booklet -> Wire -> (Booklet, Word16)
value b wire = case b Map.! wire of
    Value x -> (b, x)
    And wire1 wire2 ->
        let (b1, value1) = value b wire1
            (b2, value2) = value b1 wire2
            val = value1 Bits..&. value2
         in (Map.insert wire (Value val) b2, val)
    AndLit x wireIn ->
        let (b', valueIn) = value b wireIn
            val = x Bits..&. valueIn
         in (Map.insert wire (Value val) b', val)
    Or wire1 wire2 ->
        let (b1, value1) = value b wire1
            (b2, value2) = value b1 wire2
            val = value1 Bits..|. value2
         in (Map.insert wire (Value val) b2, val)
    LShift wireIn n ->
        let (b', valueIn) = value b wireIn
            val = valueIn `Bits.shift` n
         in (Map.insert wire (Value val) b', val)
    RShift wireIn n ->
        let (b', valueIn) = value b wireIn
            val = valueIn `Bits.shift` (- n)
         in (Map.insert wire (Value val) b', val)
    Not wireIn ->
        let (b', valueIn) = value b wireIn
            val = Bits.complement valueIn
         in (Map.insert wire (Value val) b', val)
    Direct wireIn ->
        let (b', valueIn) = value b wireIn
         in (Map.insert wire (Value valueIn) b', valueIn)
