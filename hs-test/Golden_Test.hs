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
import Utilities (applySolvers)

problems :: [((Text -> Text, Text -> Text), String)]
problems =
    [ (AOC_2015_01.solvers, "2015_01")
    , (AOC_2015_02.solvers, "2015_02")
    , (AOC_2015_03.solvers, "2015_03")
    ]

problemToTest :: ((Text -> Text, Text -> Text), String) -> TestTree
problemToTest (slv, name) = goldenVsString name goldenFile runTest
  where
    goldenFile = "../aoc-data/good/" ++ name ++ ".txt"
    runTest :: IO LBS.ByteString
    runTest = applySolvers slv name <&> LBS.fromStrict . encodeUtf8

test_golden :: IO TestTree
test_golden = do
    return $ testGroup "Haskell AOC Golden tests" (map problemToTest problems)
