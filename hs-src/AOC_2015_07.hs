{- |
 Module      : AOC_2015_07
 Description : Advent of code 2015 day 7
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_07 (solvers, booklet, value) where

import Data.Bits (complement, shift, (.&.), (.|.))
import Data.Functor (($>))
import Data.List qualified as L (foldl')
import Data.Map (Map)
import Data.Map qualified as Map (empty, insert, lookup)
import Data.Text (Text)
import Data.Text qualified as T (lines, pack)
import Data.Word (Word16)
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as M (some, try)
import Text.Megaparsec.Char qualified as MC (lowerChar)

import Utilities (lexeme, pSymbol, pUnsignedInt, parseOrStop)

solvers :: (Text -> Text, Text -> Text)
solvers =
    ( T.pack . show . (`value` "a") . booklet . T.lines
    , const "NYI"
    )

type Wire = Text

data Connection
    = Value Word16
    | And Wire Wire
    | And' Word16 Wire
    | Or Wire Wire
    | LShift Wire Int
    | RShift Wire Int
    | Not Wire
    | Direct Wire

type Booklet = Map Wire Connection

booklet :: [Text] -> Booklet
booklet = L.foldl' (\m (w, c) -> Map.insert w c m) Map.empty . map instruction

instruction :: Text -> (Wire, Connection)
instruction = parseOrStop pInstruction
  where
    pInstruction =
        M.try pDirect
            <|> M.try pBinary
            <|> M.try pBinary'
            <|> M.try pShift
            <|> M.try pUnary
            <|> pValue
    pWire = lexeme (T.pack <$> M.some MC.lowerChar)
    pValue = do
        input <- fromIntegral <$> pUnsignedInt
        _ <- pSymbol "->"
        output <- pWire
        return (output, Value input)
    pDirect = do
        input <- pWire
        _ <- pSymbol "->"
        output <- pWire
        return (output, Direct input)
    pBinary = do
        input1 <- pWire
        isAnd <- (pSymbol "AND" $> True) <|> (pSymbol "OR" $> False)
        input2 <- pWire
        _ <- pSymbol "->"
        output <- pWire
        return (output, (if isAnd then And else Or) input1 input2)
    pBinary' = do
        input1 <- fromIntegral <$> pUnsignedInt
        _ <- pSymbol "AND"
        input2 <- pWire
        _ <- pSymbol "->"
        output <- pWire
        return (output, And' input1 input2)
    pShift = do
        input1 <- pWire
        isLeft <- (pSymbol "LSHIFT" $> True) <|> (pSymbol "RSHIFT" $> False)
        input2 <- pUnsignedInt
        _ <- pSymbol "->"
        output <- pWire
        return (output, (if isLeft then LShift else RShift) input1 input2)
    pUnary = do
        _ <- pSymbol "NOT"
        input <- pWire
        _ <- pSymbol "->"
        output <- pWire
        return (output, Not input)

value :: Booklet -> Wire -> Word16
value b wire = case Map.lookup wire b of
    Just (Value x) -> x
    Just (And v1 v2) -> value b v1 .&. value b v2
    Just (And' x v) -> x .&. value b v
    Just (Or v1 v2) -> value b v1 .|. value b v2
    Just (LShift v n) -> value b v `shift` n
    Just (RShift v n) -> value b v `shift` (- n)
    Just (Not v) -> complement (value b v)
    Just (Direct v) -> value b v
    Nothing -> error "Internal failure"
