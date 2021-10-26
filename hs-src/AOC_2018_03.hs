module AOC_2018_03 (solvers) where

import Data.Array.Unboxed (UArray)
import Data.Array.Unboxed qualified as U (listArray, (!), (//))
import Data.Attoparsec.ByteString.Char8 qualified as A (char, decimal, parseOnly, skipSpace)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC (lines)
import Data.List qualified as L (foldl')
import Data.Text (Text)
import Data.Text qualified as T (pack)
import Data.Text.Encoding qualified as TE (encodeUtf8)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ overlaps fabric
    , T.pack . show $findUnOverlapped fabric claims
    )
  where
    claims = map parseClaim . BC.lines $ TE.encodeUtf8 t
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

{- | Parse a Claim from a string

 >>> parseClaim $ BC.pack "#1 @ 1,3: 4x4"
 Claim {idCode = 1, x = 1, y = 3, w = 4, h = 4}
-}
parseClaim :: ByteString -> Claim
parseClaim s = case A.parseOnly claimParser s of
    Left msg -> error msg
    Right claim -> claim
  where
    claimParser = do
        _ <- A.char '#'
        c_id <- A.decimal
        A.skipSpace
        _ <- A.char '@'
        A.skipSpace
        c_x <- A.decimal
        _ <- A.char ','
        c_y <- A.decimal
        _ <- A.char ':'
        A.skipSpace
        c_w <- A.decimal
        _ <- A.char 'x'
        Claim c_id c_x c_y c_w <$> A.decimal

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
