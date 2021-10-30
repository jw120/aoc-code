module AOC_2015_19_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2015_19 (Atom (..), Molecule, Replacement, readInput, stepsTo)

rules :: [Replacement]
rules =
    snd . readInput $
        "e => H\n\
        \e => O\n\
        \H => HO\n\
        \H => OH\n\
        \O => HH\n\
        \\n\
        \XYZ\n"

hoh :: Molecule
hoh = [Atom "H", Atom "O", Atom "H"]

hohoho :: Molecule
hohoho = [Atom "H", Atom "O", Atom "H", Atom "O", Atom "H", Atom "O"]

unit_2015_19_hoh_1 :: IO ()
unit_2015_19_hoh_1 = stepsTo hoh rules @?= 3

unit_2015_19_hoh_2 :: IO ()
unit_2015_19_hoh_2 = stepsTo hohoho rules @?= 6
