{- |
 Module      : AOC_2018_12
 Description : Advent of code 2018 day 12
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_12 (solvers) where

-- import Data.Map.Lazy (Map)
-- import Data.Map.Lazy qualified as Map (fromList, empty, insert, insertWith, (!))
--import Data.List qualified as L (foldl')
import Data.Text (Text)
import Data.Text qualified as T (last, lines, null, pack)
import Text.Megaparsec qualified as M (many)
import Text.Megaparsec.Char qualified as MC (char, string)

import Utilities (Parser, parseOrStop, ($>), (<|>))

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ run 20 rules initialState
    , let (rep, n, delta) = untilRepeat rules initialState
       in T.pack $ show (sumState rep) ++ " " ++ show (n, delta)
    )
  where
    ls = T.lines t
    initialState = parseOrStop pInitial $ head ls
    rules = map (parseOrStop pRule) . filter ((== '#') . T.last) $ filter (not . T.null) $ tail ls

newtype State = State [(Int, Bool)] deriving (Eq)

data Rule = Rule Bool Bool Bool Bool Bool deriving (Show)

data Chunk = Chunk Int Bool Bool Bool Bool Bool

instance Show Chunk where
    show (Chunk i a b c d e) = show i ++ ": " ++ map (\x -> if x then '#' else '.') [a, b, c, d, e]

instance Show State where
    show (State s) = show (fst (head s)) ++ ": " ++ map ((\b -> if b then '#' else '.') . snd) s

pInitial :: Parser State
pInitial = State . zip [0 ..] <$> (MC.string "initial state: " *> M.many pPlant)

pRule :: Parser Rule
pRule = toRule <$> (M.many pPlant <* MC.string " => #")
  where
    toRule [a, b, c, d, e] = Rule a b c d e
    toRule _ = error "Bad rule"

pPlant :: Parser Bool
pPlant = (MC.char '#' $> True) <|> (MC.char '.' $> False)

-- | Iterate rules once
apply :: [Rule] -> State -> State
apply rules st = State . trimEnds . map anyRuleMatches $ byChunk st
  where
    anyRuleMatches :: Chunk -> (Int, Bool)
    anyRuleMatches c@(Chunk i _ _ _ _ _) = (i, any (`ruleMatches` c) rules)
    ruleMatches :: Rule -> Chunk -> Bool
    ruleMatches (Rule a b c d e) (Chunk _ p q r s t) =
        a == p && b == q && c == r && d == s && e == t

-- | Remove False's from front and back of a list
trimEnds :: [(Int, Bool)] -> [(Int, Bool)]
trimEnds = dropWhile (not . snd) . reverse . dropWhile (not . snd) . reverse

-- | Split a state into chunks of 5
byChunk :: State -> [Chunk]
byChunk (State s) = go (frontPadding ++ s ++ backPadding)
  where
    go (s1 : s2 : s3 : s4 : s5 : rest) = toChunk [s1, s2, s3, s4, s5] : go (s2 : s3 : s4 : s5 : rest)
    go _ = []
    frontPadding = zip [frontIndex ..] [False, False, False, False]
      where
        frontIndex = fst (head s) - 4
    backPadding = zip [backIndex ..] [False, False, False, False]
      where
        backIndex = fst (last s) + 1
    toChunk [(_, a), (_, b), (i, c), (_, d), (_, e)] = Chunk i a b c d e
    toChunk xs = error $ "Bad chunk" ++ show xs

-- | Iterate rules given number of times
run :: Int -> [Rule] -> State -> State
run n rules s
    | n > 0 = run (n - 1) rules (apply rules s)
    | n == 0 = s
    | otherwise = error "bad run"

-- | Sum of number of trues in a State
sumState :: State -> Int
sumState (State s) = sum . map fst $ filter snd s

-- | Iterate until a state's plants repeats (irrespective of numbering)
untilRepeat :: [Rule] -> State -> (State, Int, Int)
untilRepeat rules s = go 1 s (apply rules s)
  where
    go i s1@(State x1) s2@(State x2)
        | map snd x1 == map snd x2 = (s1, i, fst (head x2) - fst (head x1))
        | otherwise = go (i + 1) s2 (apply rules s2)
