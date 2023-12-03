// Advent of Code, 2022 day 2

use aoc_rust::stdin_lines;

#[derive(PartialEq)]
enum Outcome {
    Win,
    Draw,
    Lose,
}

impl Outcome {
    fn score(&self) -> i32 {
        match self {
            Outcome::Win => 6,
            Outcome::Draw => 3,
            Outcome::Lose => 0,
        }
    }
}

enum Shape {
    Rock,
    Paper,
    Scissors,
}

impl Shape {
    fn resolve(&self, other: &Self) -> Outcome {
        match (self, other) {
            (Shape::Rock, Shape::Scissors) => Outcome::Win,
            (Shape::Rock, Shape::Paper) => Outcome::Lose,
            (Shape::Paper, Shape::Rock) => Outcome::Win,
            (Shape::Paper, Shape::Scissors) => Outcome::Lose,
            (Shape::Scissors, Shape::Paper) => Outcome::Win,
            (Shape::Scissors, Shape::Rock) => Outcome::Lose,
            _ => Outcome::Draw,
        }
    }

    fn score(&self) -> i32 {
        match self {
            Shape::Rock => 1,
            Shape::Paper => 2,
            Shape::Scissors => 3,
        }
    }

    fn parse(c: char) -> Shape {
        match c {
            'A' => Shape::Rock,
            'B' => Shape::Paper,
            'C' => Shape::Scissors,
            _ => {
                panic!("Unknown shape");
            }
        }
    }

    fn next(&self) -> Shape {
        match self {
            Shape::Rock => Shape::Paper,
            Shape::Paper => Shape::Scissors,
            Shape::Scissors => Shape::Rock,
        }
    }
}

enum Response {
    X,
    Y,
    Z,
}

impl Response {
    fn parse(c: char) -> Response {
        match c {
            'X' => Response::X,
            'Y' => Response::Y,
            'Z' => Response::Z,
            _ => {
                panic!("Unknown response");
            }
        }
    }
    fn to_shape(&self) -> Shape {
        match self {
            Response::X => Shape::Rock,
            Response::Y => Shape::Paper,
            Response::Z => Shape::Scissors,
        }
    }
    fn to_outcome(&self) -> Outcome {
        match self {
            Response::X => Outcome::Lose,
            Response::Y => Outcome::Draw,
            Response::Z => Outcome::Win,
        }
    }
}

struct Game {
    opponent: Shape,
    response: Response,
}

impl Game {
    fn parse(s: &str) -> Game {
        assert!(s.len() == 3);
        Game {
            opponent: Shape::parse(s.chars().nth(0).expect("missing shape")),
            response: Response::parse(s.chars().nth(2).expect("missing response")),
        }
    }
}

fn score_a(g: &Game) -> i32 {
    let player = g.response.to_shape();
    let outcome = player.resolve(&g.opponent);
    outcome.score() + player.score()
}

fn score_b(g: &Game) -> i32 {
    let outcome = g.response.to_outcome();
    let mut player = Shape::Rock;
    while player.resolve(&g.opponent) != outcome {
        player = player.next()
    }
    outcome.score() + player.score()
}

fn main() {
    let games: Vec<Game> = stdin_lines().map(|s| Game::parse(&s)).collect();

    let part_a: i32 = games.iter().map(score_a).sum();
    let part_b: i32 = games.iter().map(score_b).sum();

    println!("{}", part_a);
    println!("{}", part_b);
}
