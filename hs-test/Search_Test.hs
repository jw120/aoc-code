module Search_Test where

import Test.Tasty.HUnit ((@?=))

import Data.Array qualified as A (array, (!))
import Data.Bifunctor qualified as Bifunctor (second)
import Search (bfsBasic, bfsVariable)

type MazePosition = (Int, Int)

-- Very simple maze to test path for bfsBasic
maze1 :: [String]
maze1 =
    [ "#..F"
    , "S.##"
    ]

unit_basic_maze_1 :: IO ()
unit_basic_maze_1 =
    let (moves, _, start, finish) = mkMaze maze1
     in bfsBasic moves finish start @?= Just (4, [(3, 0), (2, 0), (1, 0), (1, 1), (0, 1)])

unit_variable_maze_1 :: IO ()
unit_variable_maze_1 =
    let (_, moves, start, finish) = mkMaze maze1
     in Bifunctor.second length <$> bfsVariable moves 3 finish start @?= Just (7, 4)

-- Very simple maze to test bfsBasic
maze2 :: [String]
maze2 =
    [ "....F"
    , ".####"
    , "....."
    , "##.#."
    , ".S..."
    ]

unit_basic_maze_2 :: IO ()
unit_basic_maze_2 =
    let (moves, _, start, finish) = mkMaze maze2
     in Bifunctor.second length <$> bfsBasic moves finish start @?= Just (11, 12)

unit_variable_maze_2 :: IO ()
unit_variable_maze_2 =
    let (_, moves, start, finish) = mkMaze maze2
     in bfsVariable moves 3 finish start @?= Just (18, [(4, 0), (3, 0), (2, 0), (1, 0), (0, 1), (1, 2), (2, 3), (1, 4)])

-- Very simple maze with no solution to test bfsBasic
maze3 :: [String]
maze3 =
    [ "...#F"
    , ".#.##"
    , "....."
    , "##.#."
    , ".S..."
    ]
unit_basic_maze_3 :: IO ()
unit_basic_maze_3 =
    let (moves, _, start, finish) = mkMaze maze3
     in bfsBasic moves finish start @?= Nothing

unit_variable_maze_3 :: IO ()
unit_variable_maze_3 =
    let (_, moves, start, finish) = mkMaze maze3
     in bfsVariable moves 3 finish start @?= Nothing

-- Provide drivers for mazes with bfsBasic and bfsVariable
mkMaze :: [String] -> (MazePosition -> [MazePosition], Int -> MazePosition -> [MazePosition], MazePosition, MazePosition)
mkMaze xs = (movesBasic, movesVariable, start, finish)
  where
    tagged :: [(MazePosition, Char)] =
        [((i, j), c) | (j, row) <- zip [0 ..] xs, (i, c) <- zip [0 ..] row]
    grid = A.array ((0, 0), (width - 1, height -1)) $ map (\(ix, c) -> (ix, c /= '#')) tagged
    width = length $ head xs
    height = length xs
    start = fst . head $ filter ((== 'S') . snd) tagged
    finish = fst . head $ filter ((== 'F') . snd) tagged
    -- basic moves: N/E/S/W
    movesBasic :: MazePosition -> [MazePosition]
    movesBasic p = filter open . filter inBounds $ simpleAdjacents p
    -- variable length moves. N/E/S/W moves have length 2, diagonal moves have length 3
    movesVariable :: Int -> MazePosition -> [MazePosition]
    movesVariable 2 p = filter open . filter inBounds $ simpleAdjacents p
    movesVariable 3 p = filter open . filter inBounds $ adjacents p
      where
        adjacents (i, j) = [(i - 1, j - 1), (i + 1, j - 1), (i - 1, j + 1), (i + 1, j + 1)]
    movesVariable _ _ = []
    simpleAdjacents (i, j) = [(i - 1, j), (i, j - 1), (i + 1, j), (i, j + 1)]
    inBounds (i, j) = i >= 0 && i < width && j >= 0 && j < height
    open (i, j) = grid A.! (i, j)
