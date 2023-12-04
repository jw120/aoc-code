// Advent of Code, 2023 day 02

use aoc_rust::stdin_lines;

// Number of red, green, blue balls
struct Subset {
    red: u32,
    green: u32,
    blue: u32,
}

impl Subset {
    fn new<S: AsRef<str>>(s: S) -> Subset {
        let mut red: u32 = 0;
        let mut green: u32 = 0;
        let mut blue: u32 = 0;
        for piece in s.as_ref().split(", ") {
            let pieces: Vec<&str> = piece.split_whitespace().collect();
            match pieces[..] {
                [n, "red"] => {
                    red = n.parse().unwrap();
                }
                [n, "green"] => {
                    green = n.parse().unwrap();
                }
                [n, "blue"] => {
                    blue = n.parse().unwrap();
                }
                _ => {
                    panic!("Unknown piece {}", piece)
                }
            }
        }
        Subset { red, green, blue }
    }

    fn power(&self) -> u32 {
        self.red * self.green * self.blue
    }
}

struct Game {
    id: u32,
    subsets: Vec<Subset>,
}

impl Game {
    fn new<S: AsRef<str>>(s: S) -> Game {
        let s_remaining: &str = s.as_ref().strip_prefix("Game").unwrap().trim();
        let mut colon_split = s_remaining.split(":");
        let s_id: &str = colon_split.next().unwrap();
        let s_subsets: &str = colon_split.next().unwrap();
        assert!(colon_split.next() == None);
        Game {
            id: s_id.parse().unwrap(),
            subsets: s_subsets.split("; ").map(Subset::new).collect(),
        }
    }
}

// Test if part is a subset of the full number of balls
fn possible(full: &Subset, part: &Subset) -> bool {
    part.red <= full.red && part.green <= full.green && part.blue <= full.blue
}

// Test if all subsets in a game are possible with given full number of balls
fn all_possible(full: &Subset, game: &Game) -> bool {
    game.subsets.iter().all(|s| possible(full, s))
}

// Return smallest numbers of balls that could have generated the game."""
fn fewest(game: &Game) -> Subset {
    Subset {
        red: game.subsets.iter().map(|s| s.red).max().unwrap(),
        green: game.subsets.iter().map(|s| s.green).max().unwrap(),
        blue: game.subsets.iter().map(|s| s.blue).max().unwrap(),
    }
}

fn main() {
    let games: Vec<Game> = stdin_lines().map(Game::new).collect();
    let full = Subset {
        red: 12,
        green: 13,
        blue: 14,
    };

    let part_a: u32 = games
        .iter()
        .filter(|g| all_possible(&full, g))
        .map(|g| g.id)
        .sum();

    let part_b: u32 = games.iter().map(|g| Subset::power(&fewest(g))).sum();

    println!("{}", part_a);
    println!("{}", part_b);
}
