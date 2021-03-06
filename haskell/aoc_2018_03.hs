module AOC_2018_03 where

import Data.Array.Unboxed
  ( UArray,
    listArray,
    (!),
    (//),
  )
import Data.Attoparsec.ByteString.Char8
  ( char,
    decimal,
    parseOnly,
    skipSpace,
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Foldable (foldl')

-- Size of the fabric
fabricSize :: Int
fabricSize = 1000

type Fabric = UArray (Int, Int) Int

data Claim = Claim
  { idCode :: Int, -- avoid name 'id' as clashes with preulde
    x :: Int, -- distance from left edge
    y :: Int, -- distance from top edge
    w :: Int, -- width
    h :: Int -- height
  }
  deriving (Show)

-- | Parse a Claim from a string
--
-- >>> parseClaim $ BC.pack "#1 @ 1,3: 4x4"
-- Claim {idCode = 1, x = 1, y = 3, w = 4, h = 4}
parseClaim :: ByteString -> Claim
parseClaim s = case parseOnly claimParser s of
  Left msg -> error msg
  Right claim -> claim
  where
    claimParser = do
      _ <- char '#'
      c_id <- decimal
      skipSpace
      _ <- char '@'
      skipSpace
      c_x <- decimal
      _ <- char ','
      c_y <- decimal
      _ <- char ':'
      skipSpace
      c_w <- decimal
      _ <- char 'x'
      Claim c_id c_x c_y c_w <$> decimal

-- Main function for part (a) - number of overlapping squares on the fabric
overlaps :: Fabric -> Int
overlaps f =
  sum
    [ isOverlap (f ! (i, j))
      | i <- [0 .. fabricSize - 1],
        j <- [0 .. fabricSize - 1]
    ]
  where
    isOverlap :: Int -> Int
    isOverlap i = if i > 1 then 1 else 0

emptyFabric :: Fabric
emptyFabric = listArray ((0, 0), (fabricSize - 1, fabricSize - 1)) (repeat 0)

addClaim :: Fabric -> Claim -> Fabric
addClaim a (Claim _ c_x c_y c_w c_h) =
  a
    // [ ((i, j), a ! (i, j) + 1)
         | i <- [c_x .. c_x + c_w - 1],
           j <- [c_y .. c_y + c_h - 1]
       ]

-- Main function for part (b) - return the claim that has no overlappes
findUnoverlapped :: Fabric -> [Claim] -> Int
findUnoverlapped fabric = idCode . head . filter isUnoverlapped
  where
    isUnoverlapped :: Claim -> Bool
    isUnoverlapped (Claim _ c_x c_y c_w c_h) = all (== 1) patch
      where
        patch = [fabric ! (i, j) | i <- [c_x .. c_x + c_w - 1], j <- [c_y .. c_y + c_h - 1]]

main :: IO ()
main = do
  input_lines <- BC.lines <$> BC.getContents
  let claims = map parseClaim input_lines
  let fabric = foldl' addClaim emptyFabric claims
  print $ overlaps fabric
  print $ findUnoverlapped fabric claims
