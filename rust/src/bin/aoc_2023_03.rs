// Advent of Code, 2023 day 03

use aoc_rust::Coord;

struct Number {
    value: isize,
    position: Coord, // position of first digit
    length: i32,     // number of digits
}

impl Number {
    // Is the number adjacent to the coordinate
    fn is_adjacent(&self, c: Coord) -> bool {
        c.col >= self.position.col - 1
            && c.col <= self.position.col + self.length
            && c.row >= self.position.row - 1
            && c.row <= self.position.row + 1
    }

    // Is the number adjacent to any of the coordinates
    fn is_any_adjacent(&self, cs: &[Coord]) -> bool {
        cs.iter().any(|c| self.is_adjacent(*c))
    }
}

fn gear_ratios(stars: &[Coord], numbers: &[Number]) -> isize {
    let mut sum = 0;
    for star in stars {
        let adjacent_numbers: Vec<&Number> =
            numbers.iter().filter(|n| n.is_adjacent(*star)).collect();
        match adjacent_numbers[..] {
            [n1, n2] => {
                sum += n1.value * n2.value;
            }
            _ => {}
        }
    }
    sum
}

fn read_schematic() -> (Vec<Number>, Vec<Coord>, Vec<Coord>) {
    (Vec::new(), Vec::new(), Vec::new())

    // def __init__(self, lines: list[str]) -> None:
    //     # Read grid data
    //     self.extent: Extent = Extent(x=len(lines[0]), y=len(lines))
    //     self.data: dict[Coord, str] = {}
    //     for y, line in enumerate(lines):
    //         for x, c in enumerate(line):
    //             self.data[Coord(x, y)] = c
    //     # Locate symbols
    //     self.symbols: list[Coord] = []
    //     for c in self.extent.upto():
    //         if (not self.data[c].isdigit()) and self.data[c] != ".":
    //             self.symbols.append(c)
    //     # Locate numbers
    //     self.numbers: list[Number] = []
    //     n: Number | None = None
    //     for y in range(self.extent.y):
    //         for x in range(self.extent.x + 1):
    //             if x == self.extent.x:
    //                 if n is not None:
    //                     self.numbers.append(n)
    //                 n = None
    //             else:
    //                 c = Coord(x, y)
    //                 d = self.data[c]
    //                 if d.isdigit():
    //                     if n is None:
    //                         n = Number(value=int(d), position=c, length=1)
    //                     else:
    //                         n.value = n.value * 10 + int(d)
    //                         n.length += 1
    //                 else:
    //                     if n is not None:
    //                         self.numbers.append(n)
    //                     n = None
}

fn main() {
    let (numbers, symbols, stars) = read_schematic();

    let part_a: isize = numbers
        .iter()
        .filter(|n| (*n).is_any_adjacent(&symbols))
        .map(|n| n.value)
        .sum();

    println!("{}", part_a);
    println!("{}", gear_ratios(&stars, &numbers));
}
