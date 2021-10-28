module Golden_Test (test_golden) where

import Data.ByteString.Lazy qualified as LBS
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import AOC_2015_01 (solvers)
import AOC_2015_02 (solvers)
import AOC_2015_03 (solvers)
import AOC_2015_04 (solvers)
import AOC_2015_05 (solvers)
import AOC_2015_06 (solvers)
import AOC_2015_07 (solvers)
import AOC_2015_08 (solvers)
import AOC_2015_09 (solvers)
import AOC_2015_10 (solvers)
import AOC_2015_11 (solvers)
import AOC_2015_12 (solvers)
import AOC_2015_13 (solvers)

import AOC_2015_14 (solvers)

import AOC_2015_15 (solvers)

-- import AOC_2015_16 (solvers)
-- import AOC_2015_17 (solvers)
-- import AOC_2015_18 (solvers)
-- import AOC_2015_19 (solvers)
-- import AOC_2015_20 (solvers)
-- import AOC_2015_21 (solvers)
-- import AOC_2015_22 (solvers)
-- import AOC_2015_23 (solvers)
-- import AOC_2015_24 (solvers)
-- import AOC_2015_25 (solvers)

import AOC_2018_01 (solvers)
import AOC_2018_02 (solvers)
import AOC_2018_03 (solvers)
import AOC_2018_04 (solvers)
import AOC_2018_05 (solvers)
import AOC_2018_06 (solvers)

-- import AOC_2018_07 (solvers)
-- import AOC_2018_08 (solvers)
-- import AOC_2018_09 (solvers)
-- import AOC_2018_10 (solvers)

import Utilities (applySolvers)

problems :: [(Text -> (Text, Text), String)]
problems =
    [ (AOC_2015_01.solvers, "2015_01")
    , (AOC_2015_02.solvers, "2015_02")
    , (AOC_2015_03.solvers, "2015_03")
    , (AOC_2015_04.solvers, "2015_04")
    , (AOC_2015_05.solvers, "2015_05")
    , (AOC_2015_06.solvers, "2015_06")
    , (AOC_2015_07.solvers, "2015_07")
    , (AOC_2015_08.solvers, "2015_08")
    , (AOC_2015_09.solvers, "2015_09")
    , (AOC_2015_10.solvers, "2015_10")
    , (AOC_2015_11.solvers, "2015_11")
    , (AOC_2015_12.solvers, "2015_12")
    , (AOC_2015_13.solvers, "2015_13")
    , (AOC_2015_14.solvers, "2015_14")
    , (AOC_2015_15.solvers, "2015_15")
    , -- , (AOC_2015_16.solvers, "2015_16")
      -- , (AOC_2015_17.solvers, "2015_17")
      -- , (AOC_2015_18.solvers, "2015_18")
      -- , (AOC_2015_19.solvers, "2015_19")
      -- , (AOC_2015_20.solvers, "2015_20")
      -- , (AOC_2015_21.solvers, "2015_21")
      -- , (AOC_2015_22.solvers, "2015_22")
      -- , (AOC_2015_23.solvers, "2015_23")
      -- , (AOC_2015_24.solvers, "2015_24")
      -- , (AOC_2015_25.solvers, "2015_25")
      (AOC_2018_01.solvers, "2018_01")
    , (AOC_2018_02.solvers, "2018_02")
    , (AOC_2018_03.solvers, "2018_03")
    , (AOC_2018_04.solvers, "2018_04")
    , (AOC_2018_05.solvers, "2018_05")
    , (AOC_2018_06.solvers, "2018_06")
    -- , (AOC_2018_07.solvers, "2018_07")
    -- , (AOC_2018_08.solvers, "2018_08")
    -- , (AOC_2018_09.solvers, "2018_09")
    -- , (AOC_2018_10.solvers, "2018_10")
    ]

problemToTest :: (Text -> (Text, Text), String) -> TestTree
problemToTest (slv, name) = goldenVsString name goldenFile runTest
  where
    goldenFile = "../aoc-data/good/" ++ name ++ ".txt"
    runTest :: IO LBS.ByteString
    runTest = applySolvers slv name <&> LBS.fromStrict . encodeUtf8

test_golden :: IO TestTree
test_golden = do
    return $ testGroup "Golden tests" (map problemToTest problems)
