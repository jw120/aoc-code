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
        let mut colon_split = s_remaining.split(':');
        let s_id: &str = colon_split.next().unwrap();
        let s_cards: &str = colon_split.next().unwrap();
        assert!(colon_split.next().is_none());
        let mut bar_split = s_cards.split(" | ");
        let s_winning: &str = bar_split.next().unwrap();
        let s_actual: &str = bar_split.next().unwrap();
        assert!(bar_split.next().is_none());

        Card {
            _id: s_id.parse().unwrap(),
            winning: parse_numbers(s_winning),
            actual: parse_numbers(s_actual),
        }
    }

    // return number of winning numbers for the card
    fn winners(&self) -> u32 {
        let mut count = 0;
        for n in &self.actual {
            for w in &self.winning {
                if n == w {
                    count += 1;
                    break;
                }
            }
        }
        count
    }
}

// Helper function to parse white-space separated numbers
fn parse_numbers(s: &str) -> Vec<u32> {
    s.split_whitespace().map(|x| x.parse().unwrap()).collect()
}

// Part (a) card score from number of winning cards
fn score(n: u32) -> u32 {
    if n == 0 {
        0
    } else {
        u32::pow(2, n - 1)
    }
}

// Part (b) number of cards given number of winners for each card
fn part_b(winners: &Vec<u32>) -> u32 {
    // start with one of each card
    let mut counts: Vec<u32> = vec![1; winners.len()];

    for i in 0..winners.len() {
        let i_copies: u32 = counts[i]; // *counts.get(i).unwrap();
        for j in 1..winners[i] + 1 {
            let count: &mut u32 = counts.get_mut(i + j as usize).unwrap();
            *count += i_copies;
        }
    }
    counts.iter().sum()
}

fn main() {
    let cards: Vec<Card> = stdin_lines().map(Card::new).collect();
    let winners: Vec<u32> = cards.iter().map(Card::winners).collect();
    println!("{}", winners.iter().map(|c| score(*c)).sum::<u32>());
    println!("{}", part_b(&winners));
}
