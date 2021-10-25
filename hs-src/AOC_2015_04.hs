{- |
 Module      : AOC_2015_04
 Description : Advent of code 2015 day 4
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_04 (solvers, firstZeros) where

import Data.ByteString.Lazy qualified as LBS (fromStrict)
import Data.Digest.Pure.MD5 qualified as M (md5)
import Data.List qualified as L (replicate)
import Data.Text (Text)
import Data.Text qualified as T (pack)
import Data.Text.Encoding qualified as TE (encodeUtf8)

solvers :: (Text -> Text, Text -> Text)
solvers =
    ( T.pack . show . firstZeros 5
    , T.pack . show . firstZeros 6
    )

firstZeros :: Int -> Text -> Int
firstZeros n prefix = go 1
  where
    go x
        | take n x' == target = x
        | otherwise = go (x + 1)
      where
        target = L.replicate n '0'
        x' = show . M.md5 . LBS.fromStrict $ TE.encodeUtf8 (prefix <> T.pack (show x))
