{- |
 Module      : AOC_2015_XX
 Description : Advent of code 2015 day XX
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_11 (solvers, increment) where

--import Data.List qualified as L (foldl')
import Data.Char qualified as C (chr, ord)
import Data.Text (Text)

--import Data.Text qualified as T (lines, pack)
--import Text.Megaparsec.Char qualified as MC (char, string)

-- import Utilities (Parser, pSymbol, lexeme, pUnsignedInt, parseOrStop, ($>), (<|>))

solvers :: (Text -> Text, Text -> Text)
solvers =
    ( const "NYI"
    , const "NYI"
    )

increment :: String -> String
increment = incrementFromEnd 0
  where
    incrementFromEnd i s =
        let focusChar = s !! (length s - 1 - i)
         in case focusChar of
                'z' -> incrementFromEnd (i + 1) (replaceFromEnd i 'a' s)
                c -> replaceFromEnd i (C.chr (C.ord c + 1)) s
    replaceFromEnd i c s = take (length s - 1 - i) s ++ [c] ++ drop (length s - i) s

valid :: String -> Bool
valid s = hasIncreasingTriple && not hasConfusingLetter && hasTwoPairs
  where
    hasIncreasingTriple = undefined
    hasConfusingLetter = 'i' `elem` s || 'o' `elem` s || 'l' `elem` s
    hasTwoPairs = undefined