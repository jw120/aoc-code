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
import AOC_2015_16 (solvers)
import AOC_2015_17 (solvers)
import AOC_2015_18 (solvers)

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
import AOC_2018_07 (solvers)
import AOC_2018_08 (solvers)
import AOC_2018_09 (solvers)
import AOC_2018_10 (solvers)
import AOC_2018_11 (solvers)
import AOC_2018_12 (solvers)
import AOC_2018_13 (solvers)
import AOC_2018_14 (solvers)
import AOC_2018_15 (solvers)
import AOC_2018_16 (solvers)
import AOC_2018_17 (solvers)
import AOC_2018_18 (solvers)
import AOC_2018_19 (solvers)
import AOC_2018_20 (solvers)
import AOC_2018_21 (solvers)
import AOC_2018_22 (solvers)

-- import AOC_2018_23 (solvers)
import AOC_2018_24 (solvers)
import AOC_2018_25 (solvers)

import AOC_2021_01 (solvers)
import AOC_2021_02 (solvers)
import AOC_2021_03 (solvers)
import AOC_2021_04 (solvers)
import AOC_2021_05 (solvers)
import AOC_2021_06 (solvers)
import AOC_2021_07 (solvers)

-- import AOC_2021_08 (solvers)
-- import AOC_2021_09 (solvers)
-- import AOC_2021_10 (solvers)

import Utilities (applySolvers)

problems :: [(Text -> (Text, Text), String, String)]
problems =
    [ (AOC_2015_01.solvers, "2015_01", "fast")
    , (AOC_2015_02.solvers, "2015_02", "fast")
    , (AOC_2015_03.solvers, "2015_03", "fast")
    , (AOC_2015_04.solvers, "2015_04", "slow")
    , (AOC_2015_05.solvers, "2015_05", "fast")
    , (AOC_2015_06.solvers, "2015_06", "slow")
    , (AOC_2015_07.solvers, "2015_07", "fast")
    , (AOC_2015_08.solvers, "2015_08", "fast")
    , (AOC_2015_09.solvers, "2015_09", "fast")
    , (AOC_2015_10.solvers, "2015_10", "slow")
    , (AOC_2015_11.solvers, "2015_11", "fast")
    , (AOC_2015_12.solvers, "2015_12", "fast")
    , (AOC_2015_13.solvers, "2015_13", "fast")
    , (AOC_2015_14.solvers, "2015_14", "fast")
    , (AOC_2015_15.solvers, "2015_15", "fast")
    , (AOC_2015_16.solvers, "2015_16", "fast")
    , (AOC_2015_17.solvers, "2015_17", "fast")
    , (AOC_2015_18.solvers, "2015_18", "slow")
    , -- , (AOC_2015_19.solvers, "2015_19", "fast")
      -- , (AOC_2015_20.solvers, "2015_20", "fast")
      -- , (AOC_2015_21.solvers, "2015_21", "fast")
      -- , (AOC_2015_22.solvers, "2015_22", "fast")
      -- , (AOC_2015_23.solvers, "2015_23", "fast")
      -- , (AOC_2015_24.solvers, "2015_24", "fast")
      -- , (AOC_2015_25.solvers, "2015_25", "fast")
      (AOC_2018_01.solvers, "2018_01", "fast")
    , (AOC_2018_02.solvers, "2018_02", "fast")
    , (AOC_2018_03.solvers, "2018_03", "slow")
    , (AOC_2018_04.solvers, "2018_04", "fast")
    , (AOC_2018_05.solvers, "2018_05", "fast")
    , (AOC_2018_06.solvers, "2018_06", "fast")
    , (AOC_2018_07.solvers, "2018_07", "fast")
    , (AOC_2018_08.solvers, "2018_08", "fast")
    , (AOC_2018_09.solvers, "2018_09", "slow")
    , (AOC_2018_10.solvers, "2018_10", "fast")
    , (AOC_2018_11.solvers, "2018_11", "slow")
    , (AOC_2018_12.solvers, "2018_12", "fast")
    , (AOC_2018_13.solvers, "2018_13", "fast")
    , (AOC_2018_14.solvers, "2018_14", "slow")
    , (AOC_2018_15.solvers, "2018_15", "slow")
    , (AOC_2018_16.solvers, "2018_16", "fast")
    , (AOC_2018_17.solvers, "2018_17", "slow")
    , (AOC_2018_18.solvers, "2018_18", "fast")
    , (AOC_2018_19.solvers, "2018_19", "fast")
    , (AOC_2018_20.solvers, "2018_20", "slow")
    , (AOC_2018_21.solvers, "2018_21", "slow")
    , (AOC_2018_22.solvers, "2018_22", "slow")
    , -- , (AOC_2018_23.solvers, "2018_23", "fast")
      (AOC_2018_24.solvers, "2018_24", "fast")
    , (AOC_2018_25.solvers, "2018_25", "fast")
    , (AOC_2021_01.solvers, "2021_01", "fast")
    , (AOC_2021_02.solvers, "2021_02", "fast")
    , (AOC_2021_03.solvers, "2021_03", "fast")
    , (AOC_2021_04.solvers, "2021_04", "fast")
    , (AOC_2021_05.solvers, "2021_05", "fast")
    , (AOC_2021_06.solvers, "2021_06", "fast")
    , (AOC_2021_07.solvers, "2021_07", "fast")
    -- , (AOC_2021_08.solvers, "2021_08", "fast")
    -- , (AOC_2021_09.solvers, "2021_09", "fast")
    -- , (AOC_2021_10.solvers, "2021_10", "fast")
    ]

problemToTest :: (Text -> (Text, Text), String, String) -> TestTree
problemToTest (slv, name, speed) = goldenVsString (name ++ "_" ++ speed) goldenFile runTest
  where
    goldenFile = "../aoc-data/good/" ++ name ++ ".txt"
    runTest :: IO LBS.ByteString
    runTest = applySolvers slv name <&> LBS.fromStrict . encodeUtf8

test_golden :: IO TestTree
test_golden = do
    return $ testGroup "Golden tests" (map problemToTest problems)
