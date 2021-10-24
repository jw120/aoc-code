{- |
 Module      : Utilities
 Description : Supporting functions for advent of code solutions
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module Utilities (Parser, parseOrStop, applySolvers, pSignedInt) where

import Control.Applicative qualified as A (empty)
import Data.Text (Text)
import Data.Text qualified as T (strip)
import Data.Text.IO qualified as TIO (readFile)
import Data.Void (Void)
import Text.Megaparsec qualified as M (Parsec, eof, errorBundlePretty, parse)
import Text.Megaparsec.Char as MC (space1)
import Text.Megaparsec.Char.Lexer as ML (decimal, lexeme, signed, space)

type Parser = M.Parsec Void Text

-- | Run the given Megaparsec parser (plus EOF), stopping execution if the parser fails
parseOrStop :: Parser x -> Text -> x
parseOrStop p s = case M.parse (p <* M.eof) "" s of
    Left bundle -> error (M.errorBundlePretty bundle)
    Right x -> x

-- | Megaparsec parser for a signed integer
pSignedInt :: Parser Int
pSignedInt = ML.signed spaceConsumer integer
  where
    spaceConsumer = ML.space MC.space1 A.empty A.empty
    integer = ML.lexeme spaceConsumer ML.decimal

-- | Apply the solving functions to the given file name
applySolvers :: (Text -> Text, Text -> Text) -> String -> IO Text
applySolvers (solveA, solveB) name = do
    let inputFile = "../aoc-data/input/" ++ name ++ ".txt"
    input <- T.strip <$> TIO.readFile inputFile
    return $ solveA input <> "\n" <> solveB input <> "\n"
