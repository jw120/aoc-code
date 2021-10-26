{- |
 Module      : AOC_2015_12
 Description : Advent of code 2015 day 12
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_12 (solvers, json, JSON (..), stripRed, sumNumbers) where

--import Data.List qualified as L (foldl')
import Data.Text (Text)

import Data.Text qualified as T (pack)
import Text.Megaparsec qualified as M (between, many, satisfy, sepBy)
import Text.Megaparsec.Char qualified as MC (char)

import Utilities (Parser, lexeme, pSignedInt, parseOrStop, (<|>))

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ sumNumbers j
    , T.pack . show . sumNumbers $ stripRed j
    )
  where
    j = json t

data JSON
    = JSONArray [JSON]
    | JSONObject [(String, JSON)]
    | JSONNumber Int
    | JSONString String
    deriving (Show, Eq)

sumNumbers :: JSON -> Int
sumNumbers (JSONArray xs) = sum $ map sumNumbers xs
sumNumbers (JSONObject kvs) = sum $ map (sumNumbers . snd) kvs
sumNumbers (JSONNumber x) = x
sumNumbers (JSONString _) = 0

stripRed :: JSON -> JSON
stripRed (JSONArray xs) = JSONArray $ map stripRed xs
stripRed (JSONObject kvs)
    | JSONString "red" `elem` map snd kvs = JSONArray []
    | otherwise = JSONObject $ zip (map fst kvs) (map (stripRed . snd) kvs)
stripRed j = j

json :: Text -> JSON
json = parseOrStop pJSON

pJSON :: Parser JSON
pJSON = pArray <|> pObject <|> pNumber <|> pString
  where
    pArray = lexeme $ do
        _ <- lexeme $ MC.char '['
        xs <- pJSON `M.sepBy` lexeme (MC.char ',')
        _ <- lexeme $ MC.char ']'
        return $ JSONArray xs
    pObject = lexeme $ do
        _ <- lexeme $ MC.char '{'
        xs <- pKeyValue `M.sepBy` lexeme (MC.char ',')
        _ <- lexeme $ MC.char '}'
        return $ JSONObject xs
    pKeyValue = lexeme $ do
        JSONString k <- lexeme $ pString
        _ <- lexeme $ MC.char ':'
        v <- pJSON
        return (k, v)
    pNumber = lexeme $ JSONNumber <$> pSignedInt
    pString = lexeme $ JSONString <$> M.between (MC.char '\"') (MC.char '\"') (M.many (M.satisfy (/= '\"')))
