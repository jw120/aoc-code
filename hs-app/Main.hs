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
import AOC_2018_01 qualified (solvers)
import Utilities (applySolvers)

-- Given valid command line arguments, return input file name and appropriate solve function
parseProblem :: [String] -> Maybe (String, (Text -> Text, Text -> Text))
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
                (2018, 1) -> Just AOC_2018_01.solvers
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
