{- |
 Module      : AOC_2021_02
 Description : Advent of code 2021 day 2
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2021_02 (solvers, pCommand, applyCommand, applyCommand', Command (..)) where

import Data.Text (Text)
import Data.Text qualified as T (lines, pack)
import Text.Megaparsec.Char as MC (string)

import Utilities (Parser, pUnsignedInt, parseOrStop, (<|>))

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show . uncurry (*) $ foldr applyCommand (0, 0) commands
    , T.pack . show . (\(h, d, _) -> h * d) $ foldr applyCommand' (0, 0, 0) commands
    )
  where
    commands = map (parseOrStop pCommand) $ T.lines t

data Command
    = Forward Int
    | Down Int
    | Up Int

pCommand :: Parser Command
pCommand =
    Forward <$> (string "forward " *> pUnsignedInt)
        <|> Down <$> (string "down " *> pUnsignedInt)
        <|> Up <$> (string "up " *> pUnsignedInt)

applyCommand :: Command -> (Int, Int) -> (Int, Int)
applyCommand (Forward n) (horiz, depth) = (horiz + n, depth)
applyCommand (Down n) (horiz, depth) = (horiz, depth + n)
applyCommand (Up n) (horiz, depth) = (horiz, depth - n)

applyCommand' :: Command -> (Int, Int, Int) -> (Int, Int, Int)
applyCommand' (Forward n) (horiz, depth, aim) = (horiz + n, depth + aim * n, aim)
applyCommand' (Down n) (horiz, depth, aim) = (horiz, depth, aim + n)
applyCommand' (Up n) (horiz, depth, aim) = (horiz, depth, aim - n)