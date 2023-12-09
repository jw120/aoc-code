{- |
 Module      : AOC_2015_15
 Description : Advent of code 2015 day 15
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_15 (solvers, pIngredient, Ingredient, bestCookie, cookieScore) where

import Data.List qualified as L (foldl')
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map (elems, empty, fromList, insertWith, keys, (!))
import Data.Text (Text)
import Data.Text qualified as T (lines, pack)
import Text.Megaparsec qualified as M (sepBy, some)
import Text.Megaparsec.Char qualified as MC (char, letterChar)

import Utilities (Parser, lexeme, pSignedInt, parseOrStop)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ bestCookie ingredients
    , T.pack . show $ bestCookieWithCalories 500 ingredients
    )
  where
    ingredients = map (parseOrStop pIngredient) $ T.lines t

data Ingredient = Ingredient
    { ingredientName :: Text
    , ingredientProperties :: Map Property Int
    }
    deriving (Show)
type Property = Text
type Cookie = [(Ingredient, Int)]

pIngredient :: Parser Ingredient
pIngredient = do
    name <- T.pack <$> M.some MC.letterChar <* lexeme (MC.char ':')
    properties <- pProperty `M.sepBy` lexeme (MC.char ',')
    return $ Ingredient{ingredientName = name, ingredientProperties = Map.fromList properties}

pProperty :: Parser (Property, Int)
pProperty = do
    name <- T.pack <$> lexeme (M.some MC.letterChar)
    coefficient <- pSignedInt
    return (name, coefficient)

bestCookie :: [Ingredient] -> Int
bestCookie = maximum . map cookieScore . splitOver 100

bestCookieWithCalories :: Int -> [Ingredient] -> Int
bestCookieWithCalories cals = maximum . map cookieScore . filter ((== cals) . calories) . splitOver 100

splitOver :: Int -> [x] -> [[(x, Int)]]
splitOver n [a] = [[(a, n)]]
splitOver _ [] = error "Cannot split over empty list"
splitOver n (x : xs) = [(x, i) : zs | i <- [0 .. n], zs <- splitOver (n - i) xs]

cookieScore :: Cookie -> Int
cookieScore cookie = product . map (\x -> if x < 0 then 0 else x) $ Map.elems propertySums
  where
    propertySums :: Map Property Int = L.foldl' addIngredient Map.empty cookie
    addIngredient :: Map Property Int -> (Ingredient, Int) -> Map Text Int
    addIngredient m (ingredient, teaspoons) = L.foldl' addProperty m properties
      where
        properties = filter (/= "calories") . Map.keys $ ingredientProperties ingredient
        addProperty :: Map Property Int -> Property -> Map Property Int
        addProperty m' property = Map.insertWith (+) property (teaspoons * coefficient) m'
          where
            coefficient = ingredientProperties ingredient Map.! property

calories :: Cookie -> Int
calories = L.foldl' addIngredient 0
  where
    addIngredient :: Int -> (Ingredient, Int) -> Int
    addIngredient acc (ingredient, teaspoons) = acc + teaspoons * ingredientProperties ingredient Map.! "calories"
