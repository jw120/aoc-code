{- |
 Module      : AOC_2015_07
 Description : Advent of code 2015 day 7
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_08 (solvers, deEscape) where

import Data.Char qualified as C (chr)
import Data.Text (Text)
import Numeric qualified as N (readHex)

import Data.Text qualified as T (length, lines, pack)
import Text.Megaparsec qualified as M (many, satisfy, try)
import Text.Megaparsec.Char qualified as MC (char, hexDigitChar)

import Utilities (parseOrStop, (<|>))

solvers :: (Text -> Text, Text -> Text)
solvers =
    ( T.pack . show . partA . T.lines
    , const "NYI"
    )

partA :: [Text] -> Int
partA ts = sum (map T.length ts) - sum (map (T.length . deEscape) ts)

deEscape :: Text -> Text
deEscape = parseOrStop pEscapedString
  where
    pEscapedString = T.pack <$> (MC.char '\"' *> M.many pStringChar <* MC.char '\"')
    pStringChar = M.try pSlash <|> M.try pQuote <|> M.try pHex <|> pRegular
    pSlash = MC.char '\\' *> MC.char '\\'
    pQuote = MC.char '\\' *> MC.char '\"'
    pHex = do
        _ <- MC.char '\\'
        _ <- MC.char 'x'
        a <- MC.hexDigitChar
        b <- MC.hexDigitChar
        return . C.chr . fst . head $ N.readHex [a, b]
    pRegular = M.satisfy (\c -> c /= '\\' && c /= '\"')
