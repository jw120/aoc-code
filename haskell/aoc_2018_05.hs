module AOC_2018_05 where

import Data.Char
  ( isLower,
    toLower,
  )
import Data.List
  ( nub,
    sort,
  )

-- | Are the two character reactive (same letter, opposite case)
--
-- >>> zipWith reactive "aAaAa" "AaaAB"
-- [True,True,False,False,False]
reactive :: Char -> Char -> Bool
reactive x y = toLower x == toLower y && isLower x == not (isLower y)

-- | Run reaction on a string
-- Single pass solution borrowed from https://github.com/mstksg/advent-of-code-2018
--
-- >>> react "dabAcCaCBAcCcaDA"
-- "dabCBAcaDA"
react :: String -> String
react = foldr f []
  where
    f :: Char -> String -> String
    f c [] = [c]
    f c (x : xs)
      | reactive c x = xs
      | otherwise = c : x : xs

-- | Remove one character (upper and lower) and then react
--
-- >>> reactWithout "dabAcCaCBAcCcaDA" 'a'
-- "dbCBcD"
-- >>> reactWithout "dabAcCaCBAcCcaDA" 'B'
-- "daCAcaDA"
-- >>> reactWithout "dabAcCaCBAcCcaDA" 'c'
-- "daDA"
-- >>> reactWithout "dabAcCaCBAcCcaDA" 'D'
-- "abCBAc"
reactWithout :: [Char] -> Char -> String
reactWithout s x = react $ filter (\c -> toLower c /= toLower x) s

-- | Part b - return shortest string after reacting following removal of one letter
shortest :: String -> Int
shortest s =
  minimum . map (length . reactWithout s) . nub . sort $ map toLower s

main :: IO ()
main = do
  input <- getContents
  print . length $ react input
  print $ shortest input
