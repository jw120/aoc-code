// Advent of Code, 2023 day 22

use aoc_rust::stdin_lines;
use aoc_rust::Coord3;

type Brick = (Coord3, Coord3);

fn read_coord3(s: &str) -> Coord3 {
    let pieces: Vec<&str> = s.split(',').collect();
    match pieces[..] {
        [x, y, z] => Coord3 {
            x: x.parse().unwrap(),
            y: y.parse().unwrap(),
            z: z.parse().unwrap(),
        },
        _ => panic!("Bad coordinate '{s}'"),
    }
}

fn read_brick<S: AsRef<str>>(s: S) -> Brick {
    let (a, b) = s.as_ref().split_once('~').unwrap();
    (read_coord3(a), read_coord3(b))
}

fn main() {
    let bricks: Vec<Brick> = stdin_lines().map(read_brick).collect();

    for brick in bricks {
        println!("{:?}", brick);
    }

    let part_a: usize = 0;

    println!("{part_a}");
}
