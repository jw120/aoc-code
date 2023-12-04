// Advent of Code, 2023 day 04

use aoc_rust::stdin_lines;

#[derive(Debug)]
struct Card {
    _id: u32,
    winning: Vec<u32>,
    actual: Vec<u32>,
}

impl Card {
    fn new<S: AsRef<str>>(s: S) -> Card {
        let s_remaining: &str = s.as_ref().strip_prefix("Card").unwrap().trim();
        let mut colon_split = s_remaining.split(":");
        let s_id: &str = colon_split.next().unwrap();
        let s_cards: &str = colon_split.next().unwrap();
        assert!(colon_split.next() == None);
        let mut bar_split = s_cards.split(" | ");
        let s_winning: &str = bar_split.next().unwrap();
        let s_actual: &str = bar_split.next().unwrap();
        assert!(bar_split.next() == None);

        Card {
            _id: s_id.parse().unwrap(),
            winning: parse_numbers(s_winning),
            actual: parse_numbers(s_actual),
        }
    }

    fn score(&self) -> u32 {
        let mut current_score = 0;
        for n in &self.actual {
            for w in &self.winning {
                if n == w {
                    if current_score == 0 {
                        current_score = 1;
                    } else {
                        current_score *= 2;
                    }
                    break;
                }
            }
        }
        current_score
    }
}

// Helper function to parse white-space separated numbers
fn parse_numbers(s: &str) -> Vec<u32> {
    s.split_whitespace().map(|x| x.parse().unwrap()).collect()
}

fn main() {
    let cards: Vec<Card> = stdin_lines().map(Card::new).collect();

    let part_a: u32 = cards.iter().map(Card::score).sum();

    // for c in cards {
    //     println!("{:?}", c);
    //     println!("Score {}", c.score());
    // }

    println!("{}", part_a);
}
