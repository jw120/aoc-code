{- |
 Module      : AOC_2021_02
 Description : Advent of code 2021 day 2
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2021_02 (solvers, pCommand, applyCommand, applyCommand', Command (..)) where

import Data.List qualified as L (foldl')
import Data.Text (Text)
import Data.Text qualified as T (lines, pack)
import Text.Megaparsec.Char as MC (string)

import Utilities (Parser, pUnsignedInt, parseOrStop, (<|>))

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show . uncurry (*) $ L.foldl' applyCommand (0, 0) commands
    , T.pack . show . (\(h, d, _) -> h * d) $ L.foldl' applyCommand' (0, 0, 0) commands
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

applyCommand :: (Int, Int) -> Command -> (Int, Int)
applyCommand (horiz, depth) (Forward n) = (horiz + n, depth)
applyCommand (horiz, depth) (Down n) = (horiz, depth + n)
applyCommand (horiz, depth) (Up n) = (horiz, depth - n)

applyCommand' :: (Int, Int, Int) -> Command -> (Int, Int, Int)
applyCommand' (horiz, depth, aim) (Forward n) = (horiz + n, depth + aim * n, aim)
applyCommand' (horiz, depth, aim) (Down n) = (horiz, depth, aim + n)
applyCommand' (horiz, depth, aim) (Up n) = (horiz, depth, aim - n)