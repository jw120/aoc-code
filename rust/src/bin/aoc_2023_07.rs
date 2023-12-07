// Advent of Code, 2023 day 07

use aoc_rust::stdin_lines;
use std::cmp::Ordering;
use std::collections::HashMap;

type Card = u8; // 2.14 (11=J, Q=12, K=13, A=14)

fn parse_card(c: &char) -> Card {
    if let Some(d) = c.to_digit(10) {
        if d >= 2 {
            return d as u8;
        }
    }
    match c {
        'T' => 10,
        'J' => 11,
        'Q' => 12,
        'K' => 13,
        'A' => 14,
        _ => panic!("Unknown card: '{}'", c),
    }
}

type Hand = [Card; 5];

fn parse_hand(s: &str) -> Hand {
    if let [a, b, c, d, e] = &s.chars().collect::<Vec<char>>() as &[char] {
        [
            parse_card(a),
            parse_card(b),
            parse_card(c),
            parse_card(d),
            parse_card(e),
        ]
    } else {
        panic!("Bad hand size: '{}'", s)
    }
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
enum Type {
    HighCard,
    OnePair,
    TwoPair,
    Three,
    FullHouse,
    Four,
    Five,
}

fn hand_type(hand: &Hand) -> Type {
    let mut counts: HashMap<Card, i32> = HashMap::new();
    for card in hand {
        *counts.entry(*card).or_insert(0) += 1;
    }
    let mut counts_list: Vec<&i32> = counts.values().collect();
    counts_list.sort();

    match counts_list[..] {
        [5] => Type::Five,
        [1, 4] => Type::Four,
        [2, 3] => Type::FullHouse,
        [1, 1, 3] => Type::Three,
        [1, 2, 2] => Type::TwoPair,
        [1, 1, 1, 2] => Type::OnePair,
        [1, 1, 1, 1, 1] => Type::HighCard,
        _ => panic!("Unexpected counts: {:?} {:?}", counts_list, hand),
    }
}

fn hand_type_wild(hand: &Hand) -> Type {
    hand_type(hand)
}

fn cmp_game(
    (a_hand, a_type, _): &(Hand, Type, u64),
    (b_hand, b_type, _): &(Hand, Type, u64),
) -> Ordering {
    a_type.cmp(b_type).then_with(|| a_hand.cmp(b_hand))
}

fn parse_game(s: &str) -> (Hand, u64) {
    let parts: Vec<&str> = s.split_ascii_whitespace().collect();
    if let [hand, bid] = parts[..] {
        (parse_hand(hand), bid.parse().unwrap())
    } else {
        panic!("Bad game")
    }
}

fn winnings(games: &[(Hand, Type, u64)]) -> u64 {
    games
        .iter()
        .enumerate()
        .map(|(i, (_, _, bid))| ((i + 1) as u64) * bid)
        .sum()
}

fn main() {
    let games: Vec<(Hand, u64)> = stdin_lines().map(|s| parse_game(&s)).collect();

    // part (a)
    let mut games_no_wild: Vec<(Hand, Type, u64)> =
        games.iter().map(|(h, i)| (*h, hand_type(&h), *i)).collect();
    games_no_wild.sort_by(cmp_game);
    println!("{}", winnings(&games_no_wild));

    // part (b)
    let mut games_wild: Vec<(Hand, Type, u64)> = games
        .iter()
        .map(|(h, i)| (*h, hand_type_wild(&h), *i))
        .collect();
    games_wild.sort_by(cmp_game);
    println!("{}", winnings(&games_wild));
}
