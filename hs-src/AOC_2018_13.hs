{- |
 Module      : AOC_2018_13
 Description : Advent of code 2018 day 13
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2018_13 (solvers) where

import Data.Array (Array)
import Data.Array qualified as A (array, bounds, (!))
import Data.List qualified as L (foldl', sortBy, tails)
import Data.Maybe qualified as Maybe (catMaybes)

import Data.Text (Text)
import Data.Text qualified as T (pack, unpack)

solvers :: Text -> (Text, Text)
solvers t =
    ( T.pack . show $ runToCollision initialState
    , T.pack . show $ runToLastCart initialState
    )
  where
    initialState = readNetwork . lines $ T.unpack t

--
-- Segments of the track
--

data Segment
    = Horizontal -- '-'
    | Vertical -- '|'
    | LeftDown -- '\'
    | RightUp -- '/'
    | Intersection -- '+'
    | Empty -- ' '
    deriving (Show)

toChar :: Segment -> Char
toChar Horizontal = '-'
toChar Vertical = '|'
toChar LeftDown = '\\'
toChar RightUp = '/'
toChar Intersection = '+'
toChar Empty = ' '

fromChar :: Char -> Segment
fromChar '-' = Horizontal
fromChar '|' = Vertical
fromChar '\\' = LeftDown
fromChar '/' = RightUp
fromChar '+' = Intersection
fromChar ' ' = Empty
fromChar _ = error "Unknown segment char"

--
-- Track network, coordinates are (x, y) where x runs from 0 left to right, and y from 0 top to bottom
--

newtype Network = Network (Array (Int, Int) Segment)

instance Show Network where
    show (Network n) = init $ unlines [row y | y <- [yMin .. yMax]]
      where
        ((xMin, yMin), (xMax, yMax)) = A.bounds n
        row :: Int -> String
        row j = [toChar (n A.! (x, j)) | x <- [xMin .. xMax]]

--
-- State for each cart
--

data Cart = Cart
    { cartX :: Int
    , cartY :: Int
    , heading :: Direction
    , next :: Choice
    }

instance Show Cart where
    show c = "(" ++ show (cartX c) ++ ", " ++ show (cartY c) ++ ") " ++ [head (show (heading c))] ++ show (next c)

-- Cart's current direction
data Direction = North | East | South | West deriving (Show)

-- Cart's choice at each intersection follows a sequence
data Choice = TurnLeft | GoStraight | TurnRight

instance Show Choice where
    show TurnLeft = "L"
    show TurnRight = "R"
    show _ = "-"

iterateChoice :: Choice -> Choice
iterateChoice TurnLeft = GoStraight
iterateChoice GoStraight = TurnRight
iterateChoice TurnRight = TurnLeft

--
-- Overall state of system
--

data State = State
    { network :: Network
    , carts :: [Cart]
    }
    deriving (Show)

-- | Read the initial state
readNetwork :: [String] -> State
readNetwork xs =
    State
        { network = Network $ A.array ((0, 0), (xMax, yMax)) trackData
        , carts = map mkCart cartData
        }
  where
    xMax = maximum (map length xs) - 1
    yMax = length xs - 1
    rowData :: [(Int, ([(Int, Segment)], [(Int, Direction)]))]
    rowData = zip [0 ..] $ map readRow xs
    trackData :: [((Int, Int), Segment)]
    trackData = concatMap unpackTrackRow rowData
    unpackTrackRow :: (Int, ([(Int, Segment)], a)) -> [((Int, Int), Segment)]
    unpackTrackRow (y, (zs, _)) = map (\(x, s) -> ((x, y), s)) zs
    cartData :: [(Int, Int, Direction)]
    cartData = concatMap unpackCartRow rowData
    unpackCartRow :: (Int, (b, [(Int, Direction)])) -> [(Int, Int, Direction)]
    unpackCartRow (y, (_, zs)) = map (\(x, d) -> (x, y, d)) zs
    mkCart :: (Int, Int, Direction) -> Cart
    mkCart (x, y, d) = Cart{cartX = x, cartY = y, heading = d, next = TurnLeft}

-- | Read one row
readRow :: String -> ([(Int, Segment)], [(Int, Direction)])
readRow s = (zip [0 ..] (map readTrack s), cartIndices s)
  where
    readTrack :: Char -> Segment
    readTrack '>' = Horizontal
    readTrack '<' = Horizontal
    readTrack '^' = Vertical
    readTrack 'v' = Vertical
    readTrack c = fromChar c
    cartIndices :: String -> [(Int, Direction)]
    cartIndices = Maybe.catMaybes . zipWith readCart [0 ..]
    readCart :: Int -> Char -> Maybe (Int, Direction)
    readCart i '>' = Just (i, East)
    readCart i 'v' = Just (i, South)
    readCart i '<' = Just (i, West)
    readCart i '^' = Just (i, North)
    readCart _ _ = Nothing

--
-- Part a
--

{- | Run system until collision, return collision coordinates

 >>> runToCollision $ readNetwork testNetworkStr
 (7,3)
-}
runToCollision :: State -> (Int, Int)
runToCollision s = case tick s of
    Left pos -> pos
    Right s' -> runToCollision s'

-- Run system new state or coordinate of first collision
tick :: State -> Either (Int, Int) State
tick s = fmap (\cs -> s{carts = cs}) newCarts
  where
    n :: Network = network s
    sortedCarts :: [Cart] = sortCarts $ carts s
    newCarts :: Either (Int, Int) [Cart] = L.foldl' f (Right []) (L.tails sortedCarts)
    f :: Either (Int, Int) [Cart] -> [Cart] -> Either (Int, Int) [Cart]
    f (Left pos) _ = Left pos
    f (Right movedCarts) [] = Right movedCarts
    f (Right movedCarts) (c : unmovedCarts)
        | collides c' (movedCarts ++ unmovedCarts) = Left (cartX c', cartY c')
        | otherwise = Right (c' : movedCarts)
      where
        c' = updateCart n c

--
-- Part b
--

{- | Run system to coordinate of last surviving cart

 >>> runToLastCart False testNetwork2
 (6,4)
-}
runToLastCart :: State -> (Int, Int)
runToLastCart s = (cartX lastCart, cartY lastCart)
  where
    (lastCartsMoved, lastCartsUnmoved) = until oneCart moveCart ([], sortCarts (carts s))
    lastCart = head (lastCartsMoved ++ map (updateCart n) lastCartsUnmoved) -- Finish tick
    n :: Network = network s
    moveCart :: ([Cart], [Cart]) -> ([Cart], [Cart])
    moveCart (movedCarts, c : rest) = (movedCarts', rest')
      where
        c' = updateCart n c
        movedCarts'
            | collides c' (movedCarts ++ rest) = filter (differentPosition c') movedCarts
            | otherwise = c' : filter (differentPosition c') movedCarts
        rest' = filter (differentPosition c') rest
    moveCart (movedCarts, []) = moveCart ([], sortCarts movedCarts)
    oneCart (movedCarts, unmovedCarts) = (length movedCarts + length unmovedCarts) <= 1
    differentPosition a b = cartX a /= cartX b || cartY a /= cartY b

sortCarts :: [Cart] -> [Cart]
sortCarts = L.sortBy cmpCarts
  where
    cmpCarts :: Cart -> Cart -> Ordering
    cmpCarts c1 c2 = case compare (cartY c1) (cartY c2) of
        LT -> LT
        GT -> GT
        EQ -> compare (cartX c1) (cartX c2)

-- Does the cart have same (x, y) as any of the carts in the list
collides :: Cart -> [Cart] -> Bool
collides c = any (sameCoord c)
  where
    sameCoord c1 c2 = cartX c1 == cartX c2 && cartY c1 == cartY c2

-- | Update position of cart with one move
updateCart :: Network -> Cart -> Cart
updateCart (Network n) c = case (n A.! (cartX c, cartY c), heading c) of
    -- Horizontal
    (Horizontal, East) -> c_east
    (Horizontal, West) -> c_west
    -- Vertical
    (Vertical, South) -> c_south
    (Vertical, North) -> c_north
    --  LeftDown -- '\'
    (LeftDown, East) -> c_south
    (LeftDown, North) -> c_west
    (LeftDown, West) -> c_north
    (LeftDown, South) -> c_east
    --  | RightUp-- '/'
    (RightUp, East) -> c_north
    (RightUp, North) -> c_east
    (RightUp, West) -> c_south
    (RightUp, South) -> c_west
    -- Intersection
    (Intersection, East) ->
        ( case next c of
            TurnLeft -> c_north
            TurnRight -> c_south
            _ -> c_east
        )
            { next = iterateChoice (next c)
            }
    (Intersection, West) ->
        ( case next c of
            TurnLeft -> c_south
            TurnRight -> c_north
            _ -> c_west
        )
            { next = iterateChoice (next c)
            }
    (Intersection, North) ->
        ( case next c of
            TurnLeft -> c_west
            TurnRight -> c_east
            _ -> c_north
        )
            { next = iterateChoice (next c)
            }
    (Intersection, South) ->
        ( case next c of
            TurnLeft -> c_east
            TurnRight -> c_west
            _ -> c_south
        )
            { next = iterateChoice (next c)
            }
    (s, d) -> error $ "Bad update" ++ show s ++ " " ++ show d
  where
    c_north = c{cartY = cartY c - 1, heading = North}
    c_south = c{cartY = cartY c + 1, heading = South}
    c_west = c{cartX = cartX c - 1, heading = West}
    c_east = c{cartX = cartX c + 1, heading = East}
