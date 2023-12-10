// Advent of Code, 2023 day 10

use aoc_rust::stdin_lines;
use aoc_rust::UCoord;
use grid::Grid;

#[derive(Copy, Clone, PartialEq, Default)]
enum Pipe {
    NS,
    EW,
    NE,
    NW,
    SW,
    SE,
    #[default]
    Ground,
}

#[derive(Copy, Clone, PartialEq)]
enum Direction {
    N,
    E,
    S,
    W,
}

impl Direction {
    fn reverse(&self) -> Direction {
        match self {
            Direction::N => Direction::S,
            Direction::E => Direction::W,
            Direction::S => Direction::N,
            Direction::W => Direction::E,
        }
    }
}

fn neighbour(grid: &Grid<Pipe>, position: &UCoord, direction: Direction) -> Option<(Pipe, UCoord)> {
    if (direction == Direction::N && position.row == 0)
        || (direction == Direction::W && position.col == 0)
    {
        return None;
    }
    let neighbour_position = match direction {
        Direction::N => *position - UCoord { row: 1, col: 0 },
        Direction::E => *position + UCoord { row: 0, col: 1 },
        Direction::S => *position + UCoord { row: 1, col: 0 },
        Direction::W => *position - UCoord { row: 0, col: 1 },
    };
    grid.get(neighbour_position.row, neighbour_position.col)
        .map(|neighbour_pipe| (*neighbour_pipe, neighbour_position))
}

impl Pipe {
    fn parse(ch: char) -> Pipe {
        match ch {
            '|' => Pipe::NS,
            '-' => Pipe::EW,
            'L' => Pipe::NE,
            'J' => Pipe::NW,
            '7' => Pipe::SW,
            'F' => Pipe::SE,
            '.' => Pipe::Ground,
            'S' => Pipe::Ground,
            _ => panic!("Unknown pipe '{}'", ch),
        }
    }

    fn chr(&self) -> char {
        match self {
            Pipe::NS => '|',
            Pipe::EW => '-',
            Pipe::NE => 'L',
            Pipe::NW => 'J',
            Pipe::SW => '7',
            Pipe::SE => 'F',
            Pipe::Ground => '.',
        }
    }

    fn has_exit(&self, direction: Direction) -> bool {
        match direction {
            Direction::N => *self == Pipe::NS || *self == Pipe::NE || *self == Pipe::NW,
            Direction::E => *self == Pipe::EW || *self == Pipe::NE || *self == Pipe::SE,
            Direction::S => *self == Pipe::NS || *self == Pipe::SE || *self == Pipe::SW,
            Direction::W => *self == Pipe::EW || *self == Pipe::NW || *self == Pipe::SW,
        }
    }
}

fn read_grid(grid: &mut Grid<Pipe>) -> UCoord {
    let mut start: Option<UCoord> = None;
    for (row, line) in stdin_lines().enumerate() {
        let row: Vec<Pipe> = line
            .chars()
            .enumerate()
            .map(|(col, ch)| {
                Pipe::parse(if ch == 'S' {
                    assert!(start.is_none());
                    start = Some(UCoord { row, col });
                    '.'
                } else {
                    ch
                })
            })
            .collect();
        grid.push_row(row);
    }
    start.unwrap()
}

fn run_loop(grid: &Grid<Pipe>, start: UCoord) -> usize {
    let mut position: UCoord = start;
    let mut pipe: Pipe = Pipe::Ground;
    let mut previous_direction: Option<Direction> = None;
    let mut move_count: usize = 0;
    let mut found_move;
    loop {
        found_move = false;
        for direction in [Direction::N, Direction::E, Direction::S, Direction::W] {
            if previous_direction != Some(direction.reverse()) {
                if let Some((next_pipe, next_position)) = neighbour(grid, &position, direction) {
                    if (position == start || pipe.has_exit(direction))
                        && (next_position == start || next_pipe.has_exit(direction.reverse()))
                    {
                        found_move = true;
                        position = next_position;
                        pipe = next_pipe;
                        previous_direction = Some(direction);
                        move_count += 1;
                        break;
                    }
                }
            }
        }
        assert!(found_move);
        if position == start {
            return move_count;
        }
    }
}

fn main() {
    let mut grid: Grid<Pipe> = Grid::new(0, 0);
    let start: UCoord = read_grid(&mut grid);

    for row in grid.iter_rows() {
        let line: String = row.map(Pipe::chr).collect();
        println!("{}", line);
    }

    let part_a: usize = run_loop(&grid, start) / 2;
    let part_b: i32 = 0;

    println!("{}", part_a);
    println!("{}", part_b);
}
