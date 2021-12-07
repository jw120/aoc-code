{- |
 Module      : Main
 Description : Harness for running Haskell AOC solutions
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module Main (main) where

import Data.Text (Text)
import Data.Text.IO qualified as TIO (putStr)
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)

import AOC_2015_01 qualified (solvers)
import AOC_2015_02 qualified (solvers)
import AOC_2015_03 qualified (solvers)
import AOC_2015_04 qualified (solvers)
import AOC_2015_05 qualified (solvers)
import AOC_2015_06 qualified (solvers)
import AOC_2015_07 qualified (solvers)
import AOC_2015_08 qualified (solvers)
import AOC_2015_09 qualified (solvers)
import AOC_2015_10 qualified (solvers)
import AOC_2015_11 qualified (solvers)
import AOC_2015_12 qualified (solvers)
import AOC_2015_13 qualified (solvers)
import AOC_2015_14 qualified (solvers)
import AOC_2015_15 qualified (solvers)
import AOC_2015_16 qualified (solvers)
import AOC_2015_17 qualified (solvers)
import AOC_2015_18 qualified (solvers)
import AOC_2015_19 qualified (solvers)
import AOC_2015_20 qualified (solvers)

-- import AOC_2015_21 qualified (solvers)
-- import AOC_2015_22 qualified (solvers)
-- import AOC_2015_23 qualified (solvers)
-- import AOC_2015_24 qualified (solvers)
-- import AOC_2015_25 qualified (solvers)

import AOC_2018_01 qualified (solvers)
import AOC_2018_02 qualified (solvers)
import AOC_2018_03 qualified (solvers)
import AOC_2018_04 qualified (solvers)
import AOC_2018_05 qualified (solvers)
import AOC_2018_06 qualified (solvers)
import AOC_2018_07 qualified (solvers)
import AOC_2018_08 qualified (solvers)
import AOC_2018_09 qualified (solvers)
import AOC_2018_10 qualified (solvers)
import AOC_2018_11 qualified (solvers)
import AOC_2018_12 qualified (solvers)
import AOC_2018_13 qualified (solvers)
import AOC_2018_14 qualified (solvers)
import AOC_2018_15 qualified (solvers)
import AOC_2018_16 qualified (solvers)
import AOC_2018_17 qualified (solvers)
import AOC_2018_18 qualified (solvers)
import AOC_2018_19 qualified (solvers)
import AOC_2018_20 qualified (solvers)
import AOC_2018_21 qualified (solvers)
import AOC_2018_22 qualified (solvers)
import AOC_2018_23 qualified (solvers)
import AOC_2018_24 qualified (solvers)
import AOC_2018_25 qualified (solvers)

import AOC_2021_01 qualified (solvers)
import AOC_2021_02 qualified (solvers)
import AOC_2021_03 qualified (solvers)
import AOC_2021_04 qualified (solvers)
import AOC_2021_05 qualified (solvers)
import AOC_2021_06 qualified (solvers)

-- import AOC_2021_07 qualified (solvers)
-- import AOC_2021_08 qualified (solvers)
-- import AOC_2021_09 qualified (solvers)
-- import AOC_2021_10 qualified (solvers)

import Utilities (applySolvers)

-- Given valid command line arguments, return input file name and appropriate solve function
parseProblem :: [String] -> Maybe (String, Text -> (Text, Text))
parseProblem args = case args of
    [year, problem] ->
        let year' :: Int = read year
            problem' :: Int = read problem
            valid :: Bool = year' >= 2015 && problem' >= 1 && problem' <= 25
            problem'' :: String = if problem' < 10 then '0' : show problem' else show problem'
            name = year ++ "_" ++ problem''
            maybeSolvers = case (year', problem') of
                (2015, 1) -> Just AOC_2015_01.solvers
                (2015, 2) -> Just AOC_2015_02.solvers
                (2015, 3) -> Just AOC_2015_03.solvers
                (2015, 4) -> Just AOC_2015_04.solvers
                (2015, 5) -> Just AOC_2015_05.solvers
                (2015, 6) -> Just AOC_2015_06.solvers
                (2015, 7) -> Just AOC_2015_07.solvers
                (2015, 8) -> Just AOC_2015_08.solvers
                (2015, 9) -> Just AOC_2015_09.solvers
                (2015, 10) -> Just AOC_2015_10.solvers
                (2015, 11) -> Just AOC_2015_11.solvers
                (2015, 12) -> Just AOC_2015_12.solvers
                (2015, 13) -> Just AOC_2015_13.solvers
                (2015, 14) -> Just AOC_2015_14.solvers
                (2015, 15) -> Just AOC_2015_15.solvers
                (2015, 16) -> Just AOC_2015_16.solvers
                (2015, 17) -> Just AOC_2015_17.solvers
                (2015, 18) -> Just AOC_2015_18.solvers
                (2015, 19) -> Just AOC_2015_19.solvers
                (2015, 20) -> Just AOC_2015_20.solvers
                -- (2015, 21) -> Just AOC_2015_20.solvers
                -- (2015, 22) -> Just AOC_2015_20.solvers
                -- (2015, 23) -> Just AOC_2015_20.solvers
                -- (2015, 24) -> Just AOC_2015_20.solvers
                -- (2015, 25) -> Just AOC_2015_20.solvers
                (2018, 1) -> Just AOC_2018_01.solvers
                (2018, 2) -> Just AOC_2018_02.solvers
                (2018, 3) -> Just AOC_2018_03.solvers
                (2018, 4) -> Just AOC_2018_04.solvers
                (2018, 5) -> Just AOC_2018_05.solvers
                (2018, 6) -> Just AOC_2018_06.solvers
                (2018, 7) -> Just AOC_2018_07.solvers
                (2018, 8) -> Just AOC_2018_08.solvers
                (2018, 9) -> Just AOC_2018_09.solvers
                (2018, 10) -> Just AOC_2018_10.solvers
                (2018, 11) -> Just AOC_2018_11.solvers
                (2018, 12) -> Just AOC_2018_12.solvers
                (2018, 13) -> Just AOC_2018_13.solvers
                (2018, 14) -> Just AOC_2018_14.solvers
                (2018, 15) -> Just AOC_2018_15.solvers
                (2018, 16) -> Just AOC_2018_16.solvers
                (2018, 17) -> Just AOC_2018_17.solvers
                (2018, 18) -> Just AOC_2018_18.solvers
                (2018, 19) -> Just AOC_2018_19.solvers
                (2018, 20) -> Just AOC_2018_20.solvers
                (2018, 21) -> Just AOC_2018_21.solvers
                (2018, 22) -> Just AOC_2018_22.solvers
                (2018, 23) -> Just AOC_2018_23.solvers
                (2018, 24) -> Just AOC_2018_24.solvers
                (2018, 25) -> Just AOC_2018_25.solvers
                (2021, 1) -> Just AOC_2021_01.solvers
                (2021, 2) -> Just AOC_2021_02.solvers
                (2021, 3) -> Just AOC_2021_03.solvers
                (2021, 4) -> Just AOC_2021_04.solvers
                (2021, 5) -> Just AOC_2021_05.solvers
                (2021, 6) -> Just AOC_2021_06.solvers
                -- (2021, 7) -> Just AOC_2021_07.solvers
                -- (2021, 8) -> Just AOC_2021_08.solvers
                -- (2021, 9) -> Just AOC_2021_09.solvers
                -- (2021, 10) -> Just AOC_2021_10.solvers
                _ -> Nothing
         in case (valid, maybeSolvers) of
                (True, Just solvers) -> Just (name, solvers)
                _ -> Nothing
    _ -> Nothing

main :: IO ()
main = do
    args <- getArgs
    case parseProblem args of
        Just (name, solvers) -> applySolvers solvers name >>= TIO.putStr
        _ -> do
            progName <- getProgName
            hPutStrLn stderr $ "Usage: " ++ progName ++ " year problem"
