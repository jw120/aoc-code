{- |
 Module      : AOC_2018_03
 Description : Advent of code 2018 day 3
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_03 (solvers) where

import Data.Array.Unboxed (UArray)
import Data.Array.Unboxed qualified as U (listArray, (!), (//))
import Data.List qualified as L (foldl')
import Data.Text (Text)
import Data.Text qualified as T (lines, pack)
import Text.Megaparsec.Char qualified as MC (char, string)

import Utilities (Parser, pUnsignedInt, parseOrStop)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ overlaps fabric
    , T.pack . show $findUnOverlapped fabric claims
    )
  where
    claims = map (parseOrStop pClaim) $ T.lines t
    fabric = L.foldl' addClaim emptyFabric claims

-- Size of the fabric
fabricSize :: Int
fabricSize = 1000

type Fabric = UArray (Int, Int) Int

data Claim = Claim
    { idCode :: Int -- avoid name 'id' as clashes with prelude
    , x :: Int -- distance from left edge
    , y :: Int -- distance from top edge
    , w :: Int -- width
    , h :: Int -- height
    }
    deriving (Show)

pClaim :: Parser Claim
pClaim = do
    c_id <- MC.char '#' *> pUnsignedInt
    c_x <- MC.string "@ " *> pUnsignedInt
    c_y <- MC.char ',' *> pUnsignedInt
    c_w <- MC.string ": " *> pUnsignedInt
    c_h <- MC.char 'x' *> pUnsignedInt
    return $ Claim{idCode = c_id, x = c_x, y = c_y, w = c_w, h = c_h}

-- Main function for part (a) - number of overlapping squares on the fabric
overlaps :: Fabric -> Int
overlaps f =
    sum
        [ isOverlap (f U.! (i, j))
        | i <- [0 .. fabricSize - 1]
        , j <- [0 .. fabricSize - 1]
        ]
  where
    isOverlap :: Int -> Int
    isOverlap i = if i > 1 then 1 else 0

emptyFabric :: Fabric
emptyFabric = U.listArray ((0, 0), (fabricSize - 1, fabricSize - 1)) (repeat 0)

addClaim :: Fabric -> Claim -> Fabric
addClaim a (Claim _ c_x c_y c_w c_h) =
    a
        U.// [ ((i, j), a U.! (i, j) + 1)
             | i <- [c_x .. c_x + c_w - 1]
             , j <- [c_y .. c_y + c_h - 1]
             ]

-- Main function for part (b) - return the claim that has no overlaps
findUnOverlapped :: Fabric -> [Claim] -> Int
findUnOverlapped fabric = idCode . head . filter isUnOverlapped
  where
    isUnOverlapped :: Claim -> Bool
    isUnOverlapped (Claim _ c_x c_y c_w c_h) = all (== 1) patch
      where
        patch = [fabric U.! (i, j) | i <- [c_x .. c_x + c_w - 1], j <- [c_y .. c_y + c_h - 1]]
