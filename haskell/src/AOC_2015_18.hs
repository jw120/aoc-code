{- |
 Module      : AOC_2015_18
 Description : Advent of code 2015 day 18
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2015_18 (solvers, readGrid, countGrid, stepGrid, Grid) where

import Data.Array (Array)
import Data.Array qualified as A (array, bounds, elems, indices, listArray, (!), (//))
import Data.Text (Text)
import Data.Text qualified as T (pack, unpack)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show . countGrid . runGrid 100 $ readGrid 100 t
    , T.pack . show . countGrid . runGrid' 100 . cornersOn $ readGrid 100 t
    )

type Grid = Array (Int, Int) Bool

readGrid :: Int -> Text -> Grid
readGrid n =
    A.listArray ((0, 0), (n - 1, n -1))
        . map (== '#')
        . filter (\c -> c == '#' || c == '.')
        . T.unpack

countGrid :: Grid -> Int
countGrid = sum . map fromEnum . A.elems

stepGrid :: Grid -> Grid
stepGrid g = A.array (A.bounds g) [(ix, update (g A.! ix) (neighborCount ix)) | ix <- A.indices g]
  where
    update True n = n == 2 || n == 3
    update False n = n == 3
    neighborCount ix = sum [fromEnum (g A.! jx) | jx <- neighbors ix]
    neighbors (i, j) =
        [ (x, y)
        | x <- [max (i - 1) xMin .. min (i + 1) xMax]
        , y <- [max (j - 1) yMin .. min (j + 1) yMax]
        , (x, y) /= (i, j)
        ]
      where
        ((xMin, yMin), (xMax, yMax)) = A.bounds g

cornersOn :: Grid -> Grid
cornersOn g = g A.// [((x, y), True) | x <- [xMin, xMax], y <- [yMin, yMax]]
  where
    ((xMin, yMin), (xMax, yMax)) = A.bounds g

runGrid :: Int -> Grid -> Grid
runGrid n g
    | n > 0 = runGrid (n - 1) (stepGrid g)
    | otherwise = g

runGrid' :: Int -> Grid -> Grid
runGrid' n g
    | n > 0 = runGrid' (n - 1) (cornersOn (stepGrid g))
    | otherwise = g
