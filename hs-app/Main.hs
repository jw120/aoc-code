{- |
 Module      : Main
 Description : Harness for running Haskell AOC solutions
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module Main (main) where

import Control.Monad (unless)
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)

import AOC_2015_01 qualified (solve)

-- Given valid command line arguments, return input file name and appropriate solve function
parseProblem :: [String] -> Maybe (String, String -> (String, String))
parseProblem args = case args of
    [year, problem] ->
        let year' :: Int = read year
            problem' :: Int = read problem
            valid :: Bool = year' >= 2015 && problem' >= 1 && problem' <= 25
            problem'' :: String = if problem' < 10 then '0' : show problem' else show problem'
            fn = "../aoc-data/input/" ++ year ++ "_" ++ problem'' ++ ".txt"
            maybeSolve = case (year', problem') of
                (2015, 1) -> Just AOC_2015_01.solve
                _ -> Nothing
         in case (valid, maybeSolve) of
                (True, Just solve) -> Just (fn, solve)
                _ -> Nothing
    _ -> Nothing

main :: IO ()
main = do
    args <- getArgs
    case parseProblem args of
        Just (filename, solve) -> do
            input <- readFile filename
            let (output1, output2) = solve input
            putStrLn output1
            unless (null output2) $ putStrLn output2
        _ -> do
            progName <- getProgName
            hPutStrLn stderr $ "Usage: " ++ progName ++ " year problem"
