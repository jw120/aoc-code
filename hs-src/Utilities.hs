{- |
 Module      : Utilities
 Description : Supporting functions for advent of code solutions
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module Utilities (Parser, parseOrStop) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, parse)

type Parser = Parsec Void Text

-- | Run the given Megaparsec parser, stopping execution if the parser fails
parseOrStop :: Parser x -> Text -> x
parseOrStop p s = case parse p "" s of
    Left bundle -> error (errorBundlePretty bundle)
    Right x -> x
