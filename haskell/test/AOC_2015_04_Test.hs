module AOC_2015_04_Test where

import Data.Digest.Pure.MD5 qualified as M (md5)
import Test.Tasty.HUnit ((@?=))

import AOC_2015_04 (firstZeros)

unit_2015_04_md5_1 :: IO ()
unit_2015_04_md5_1 = take 11 (show (M.md5 "abcdef609043")) @?= "000001dbbfa"

unit_2015_04_md5_2 :: IO ()
unit_2015_04_md5_2 = take 11 (show (M.md5 "pqrstuv1048970")) @?= "000006136ef"

unit_2015_04_a_1 :: IO ()
unit_2015_04_a_1 = firstZeros 5 "abcdef" @?= 609043

unit_2015_04_a_2 :: IO ()
unit_2015_04_a_2 = firstZeros 5 "pqrstuv" @?= 1048970
