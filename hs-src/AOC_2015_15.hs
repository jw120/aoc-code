{- |
 Module      : AOC_2015_15
 Description : Advent of code 2015 day 15
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_15 (solvers) where

--import Data.List qualified as L (foldl')
import Data.Text (Text)

import Data.Text qualified as T (pack)
import Text.Megaparsec qualified as M (sepBy, some)
import Text.Megaparsec.Char qualified as MC (char, letterChar, string)

import Utilities (Parser, lexeme, pSignedInt, pSymbol, pUnsignedInt, parseOrStop, ($>), (<|>))

solvers :: Text -> (Text, Text)
solvers t =
    ( "NYI"
    , "NYI"
    )

type Property = (Text, Int)
data Ingredient = Ingredient
    { ingredientName :: Text
    , ingredientProperties :: [Property]
    }

pIngredient :: Parser Ingredient
pIngredient = do
    name <- T.pack <$> M.some MC.letterChar <* lexeme (MC.char ':')
    properties <- pProperty `M.sepBy` lexeme (MC.char ',')
    return $ Ingredient{ingredientName = name, ingredientProperties = properties}

pProperty :: Parser Property
pProperty = do
    name <- T.pack <$> lexeme (M.some MC.letterChar)
    factor <- pSignedInt
    return (name, factor)
