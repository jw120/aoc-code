{- |
 Module      : AOC_2021_18
 Description : Advent of code 2021 day 18
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2021_18 (solvers, add, addList, magnitude, readSnail, Snail (..)) where

import Data.List qualified as L (foldl1')
import Data.Text (Text)
import Data.Text qualified as T (lines, pack)

import Text.Megaparsec.Char qualified as MC (char)

import Utilities (Parser, pUnsignedInt, parseOrStop, (<|>))

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show . magnitude $ numbers
    , "NYI" -- T.pack . show $ eval packet
    )
  where
    numbers = map readSnail $ T.lines t

data Snail
    = SnailReg Int -- A regular number
    | SnailPair Snail Snail -- A pair

addList :: [Snail] -> Snail
addList = L.foldl1' add

add :: Snail -> Snail -> Snail
add (SnailPair m a b) (SnailPair n p q) = reduce $ SnailPair 0 (SnailPair (m + 1) a b) (SnailPair (n + 1) p q)
add _ _ = error "Can only add two SnailPairs"



reduce :: Int -> [Snail] -> [Snail] -> Maybe [Snail]
-- Explode a pair
reduce 4 left (Reg x: Reg y : right) = Just $ addToLast x left ++ [Reg 0] ++ addToFirst y right
reduce n left (Reg x: right)
    -- Split a number
    | x > 10 = Just $ left ++ [Reg x `div` 2, Reg x `div` 2 + if odd x then 1 else 0] ++ right
    | otherwise = reduce n (left ++ [Reg x]) right
reduce n left (Open : right) = reduce (n + 1) (left ++ [Open]) right
reduce n left (Close : right) = reduce (n - 1) (left ++ [Close]) right
reduce _ _ [] = Nothing



fromLeft :: Int -> Int -> Seq Snail -> Seq Snail -> Maybe (Seq Snail)
fromLeft level 0 left (Reg x :|> right) = fromLeft level 0 left' right'
    where
        left' = fromRight left
fromLeft level explodeValue left (Reg x :|> right)
    | explodeValue + x > 10 = let left' =
        fromLeft levels 0 left' right'





reduce :: Snail -> Snail
reduce = fst . reduce' 0 0
  where
    -- inputs are surrounding pairs and exploding value coming in, output includes exploding value coming out
    reduce' :: Int -> Int -> Snail -> (Snail, Int)
    reduce' n _x (SnailReg i)
        | i < 10 = (SnailReg i, 0)
        | otherwise = (SnailPair n (i `div` 2) (i `div` 2 + if odd i then 1 else 0), 0)
    reduce' 4 x (SnailPair (SnailReg i) (SnailReg j)) = (SnailReg 0, i, j)
    reduce' n left (SnailPair p q)
        | n < 4 =
            let (p', pLeft, pRight) = reduce' (n + 1) left p
                (q', qLeft, qRight) = reduce' (n + 1) pRight q
             in (SnailPair p' q', x'')
        | n == 4 =

magnitude :: Snail -> Int
magnitude (SnailReg x) = x
magnitude (SnailPair _ p q) = 3 * magnitude p + 2 * magnitude q

readSnail :: Text -> Snail
readSnail = parseOrStop (pSnail 0)
  where
    pSnail :: Int -> Parser Snail
    pSnail n = pReg <|> pPair n
    pPair n = do
        _ <- MC.char '['
        x <- pSnail (n + 1)
        _ <- MC.char ','
        y <- pSnail (n + 1)
        _ <- MC.char ']'
        return $ SnailPair n x y
    pReg = SnailReg <$> pUnsignedInt