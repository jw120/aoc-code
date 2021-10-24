{- |
 Module      : AOC_2015_06
 Description : Advent of code 2015 day 6
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_06 (solvers, instruction, apply1, lights1, start1) where

import Data.Array (Array)
import Data.Array qualified as A (elems, listArray, range, (!), (//))
import Data.Functor (($>))
import Data.List qualified as L (foldl')
import Data.Text (Text)
import Data.Text qualified as T (lines, pack)
import Text.Megaparsec ((<|>))
import Text.Megaparsec.Char qualified as MC (char, string)

import Utilities (Parser, pUnsignedInt, parseOrStop)

solvers :: (Text -> Text, Text -> Text)
solvers =
    ( T.pack . show . lights1 . L.foldl' apply1 start1 . map instruction . T.lines
    , T.pack . show . lights2 . L.foldl' apply2 start2 . map instruction . T.lines
    )

type Grid1 = Array (Int, Int) Bool
type Grid2 = Array (Int, Int) Int

gridSize :: Int
gridSize = 1000

start1 :: Grid1
start1 = A.listArray ((0, 0), (gridSize - 1, gridSize - 1)) $ replicate (gridSize * gridSize) False

start2 :: Grid2
start2 = A.listArray ((0, 0), (gridSize - 1, gridSize - 1)) $ replicate (gridSize * gridSize) 0

data Operation = TurnOn | TurnOff | Toggle

data Instruction = Instruction Operation (Int, Int) (Int, Int)

instruction :: Text -> Instruction
instruction = parseOrStop pInstruction
  where
    pInstruction :: Parser Instruction
    pInstruction = do
        op <- (MC.string "turn on" $> TurnOn) <|> (MC.string "turn off" $> TurnOff) <|> (MC.string "toggle" $> Toggle)
        x1 <- MC.char ' ' *> pUnsignedInt
        y1 <- MC.char ',' *> pUnsignedInt
        x2 <- MC.string "through " *> pUnsignedInt
        y2 <- MC.char ',' *> pUnsignedInt
        return $ Instruction op (x1, y1) (x2, y2)

apply1 :: Grid1 -> Instruction -> Grid1
apply1 g (Instruction op iMin iMax) = g A.// [(i, light op (g A.! i)) | i <- A.range (iMin, iMax)]
  where
    light TurnOn = const True
    light TurnOff = const False
    light Toggle = not

apply2 :: Grid2 -> Instruction -> Grid2
apply2 g (Instruction op iMin iMax) = g A.// [(i, light op (g A.! i)) | i <- A.range (iMin, iMax)]
  where
    light TurnOn x = x + 1
    light TurnOff x = if x > 0 then x - 1 else x
    light Toggle x = x + 2

lights1 :: Grid1 -> Int
lights1 = length . filter id . A.elems

lights2 :: Grid2 -> Int
lights2 = sum . A.elems
