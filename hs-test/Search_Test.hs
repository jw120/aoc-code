module Search_Test where

import Test.Tasty.HUnit ((@?=))

import Data.Array qualified as A (array, (!))
import Search (bfsBasic)

type MazePosition = (Int, Int)

-- Very simple maze to test path for bfsBasic
maze1 :: [String]
maze1 =
    [ "#..E"
    , "S.##"
    ]
maze1Path :: [MazePosition]
maze1Path = [(0,1),(1,1),(1,0),(2,0),(3,0)]

-- Very simple maze to test bfsBasic
maze2 :: [String]
maze2 =
    [ "....E"
    , ".####"
    , "....."
    , "##.#."
    , ".S..."
    ]
maze2PathLength :: Int
maze2PathLength :: Int = 12

-- Very simple maze with no solution to test bfsBasic
maze3 :: [String]
maze3 =
    [ "...#E"
    , ".#.##"
    , "....."
    , "##.#."
    , ".S..."
    ]


mkMaze :: [String] -> (MazePosition -> [MazePosition], MazePosition, MazePosition)
mkMaze xs = (moves, start, finish)
    where
        tagged :: [(MazePosition, Char)] =
            [((i, j), c) | (j, row) <- zip [0..] xs, (i, c) <- zip [0..] row]
        grid = A.array ((0, 0), (width - 1, height -1)) $ map (\(ix, c) -> (ix, c /= '#')) tagged
        width = length $ head xs
        height = length xs
        start = fst . head $ filter ((== 'S') . snd) tagged
        finish = fst . head $ filter ((== 'E') . snd) tagged
        moves :: MazePosition -> [MazePosition]
        moves p = filter open . filter inBounds $ adjacents p
            where
                adjacents (i, j) = [(i - 1, j), (i, j - 1), (i + 1, j), (i, j + 1)]
                inBounds (i, j) = i >=0 && i < width && j >=0 && j < height
                open (i, j) = grid A.! (i, j)


unit_search_maze_1 :: IO ()
unit_search_maze_1 =
    let (moves, start, finish) = mkMaze maze1
    in bfsBasic moves start finish @?= Just maze1Path

unit_search_maze_2 :: IO ()
unit_search_maze_2 =
    let (moves, start, finish) = mkMaze maze2
    in length <$> bfsBasic moves start finish @?= Just maze2PathLength


unit_search_maze_3 :: IO ()
unit_search_maze_3 =
    let (moves, start, finish) = mkMaze maze3
    in bfsBasic moves start finish @?= Nothing