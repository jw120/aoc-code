{- |
 Module      : AOC_2015_19
 Description : Advent of code 2015 day 19
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_19 (solvers, stepsTo, readInput, Atom (..), Molecule, Replacement) where

import Data.List qualified as L (foldl', inits, tails)
import Data.Set (Set)
import Data.Set qualified as Set (difference, empty, filter, fromList, member, null, singleton, size, toList, union, unions)
import Data.Text (Text)
import Data.Text qualified as T (cons, lines, null, pack, singleton, strip, unpack)

--import Debug.Trace (trace)
import Text.Megaparsec qualified as M (optional, some)
import Text.Megaparsec.Char qualified as MC (letterChar, lowerChar, string)

import Utilities (Parser, parseOrStop)

trace :: p1 -> p2 -> p2
trace _ x = x

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ uncurry numMolecules $ readInput t
    , T.pack . show $ uncurry stepsTo $ readInput t
    )

newtype Atom = Atom Text deriving (Eq, Ord, Show)
type Molecule = [Atom]
type Replacement = (Atom, Molecule)

-- Number of distinct molecules reached by one replacement
numMolecules :: Molecule -> [Replacement] -> Int
numMolecules molecule = Set.size . L.foldl' addReplacements Set.empty
  where
    addReplacements :: Set Molecule -> (Atom, Molecule) -> Set Molecule
    addReplacements s (a, m) = s `Set.union` replacements (a, m) molecule

-- Molecules reached by applying one replacement
replacements :: (Atom, Molecule) -> Molecule -> Set Molecule
replacements (target, replacement) m = Set.fromList . concatMap replace $ zip (L.inits m) (L.tails m)
  where
    replace :: (Molecule, Molecule) -> [Molecule]
    replace (h, t : ts)
        | t == target = [h ++ replacement ++ ts]
        | otherwise = []
    replace (_, []) = []

-- bfs search to find
stepsTo :: Molecule -> [Replacement] -> Int
stepsTo target rules = go 0 start Set.empty
  where
    start :: Set Molecule = Set.singleton $ [Atom "e"]
    go :: Int -> Set Molecule -> Set Molecule -> Int
    go stepsDone frontier visited
        | target `Set.member` frontier = stepsDone
        | Set.null frontier = error "Failed - empty frontier"
        | otherwise = trace (showMoleculeSet frontier) $ go (stepsDone + 1) frontier''' visited'
      where
        visited' = visited `Set.union` frontier
        frontier' = Set.unions [trace (show (r, f)) $ replacements r f | r <- rules, f <- Set.toList frontier]
        frontier'' = frontier' `Set.difference` visited'
        frontier''' = Set.filter ((<= length target) . length) frontier''

showMoleculeSet :: Set Molecule -> String
showMoleculeSet = show . map showMolecule . Set.toList

showMolecule :: [Atom] -> String
showMolecule = concatMap (\(Atom x) -> T.unpack x)

readInput :: Text -> (Molecule, [Replacement])
readInput t = (parseOrStop pMolecule (last ls), map (parseOrStop pReplacement) (init ls))
  where
    ls = filter (not . T.null) . map T.strip $ T.lines t

pAtom :: Parser Atom
pAtom = do
    a <- MC.letterChar
    b <- M.optional MC.lowerChar
    return $ case b of
        Just x -> Atom $ a `T.cons` T.singleton x
        Nothing -> Atom $ T.singleton a

pMolecule :: Parser Molecule
pMolecule = M.some pAtom

pReplacement :: Parser (Atom, Molecule)
pReplacement = do
    atom <- pAtom
    molecule <- MC.string " => " *> pMolecule
    return (atom, molecule)
