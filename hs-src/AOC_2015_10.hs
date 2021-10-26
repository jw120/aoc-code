{- |
 Module      : AOC_2015_10
 Description : Advent of code 2015 day 10
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_10 (solvers, lookSay) where

--import Data.Functor (($>))
import Data.List qualified as L (span)
import Data.Text (Text)

import Data.Text qualified as T (pack, unpack)

--import Text.Megaparsec ((<|>))
--import Text.Megaparsec.Char qualified as MC (char, string)

-- import Utilities (Parser, pSymbol, lexeme, pUnsignedInt, parseOrStop, ($>), (<|>))

solvers :: (Text -> Text, Text -> Text)
solvers =
    ( T.pack . show . length . (!! 40) . iterate lookSay . map (\c -> read [c]) . T.unpack
    , T.pack . show . length . (!! 50) . iterate lookSay . map (\c -> read [c]) . T.unpack
    )

lookSay :: [Int] -> [Int]
lookSay s@(x : _) =
    let (front, rest) = L.span (== x) s
     in [length front, x] ++ lookSay rest
lookSay [] = []