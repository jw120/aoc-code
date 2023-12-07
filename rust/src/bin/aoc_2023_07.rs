// Advent of Code, 2023 day 07

use aoc_rust::stdin_lines;
use std::cmp::Ordering;
use std::collections::HashMap;

type Card = u8; // 2.14 (11=J, Q=12, K=13, A=14)

fn parse_card(c: char) -> Card {
    if let Some(d) = c.to_digit(10) {
        if d >= 2 {
            return u8::try_from(d).unwrap();
        }
    }
    match c {
        'T' => 10,
        'J' => 11,
        'Q' => 12,
        'K' => 13,
        'A' => 14,
        _ => panic!("Unknown card: '{c}'"),
    }
}

type Hand = [Card; 5];

fn parse_hand(s: &str) -> Hand {
    if let [a, b, c, d, e] = &s.chars().collect::<Vec<char>>() as &[char] {
        [
            parse_card(*a),
            parse_card(*b),
            parse_card(*c),
            parse_card(*d),
            parse_card(*e),
        ]
    } else {
        panic!("Bad hand size: '{s}'")
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

// Helper function used by hand_type and hand_type_wild
// Type of 5-card hand given a sorted list of the counts of each Card
fn type_counts(counts: &[&i32]) -> Type {
    match counts[..] {
        [5] => Type::Five,
        [1, 4] => Type::Four,
        [2, 3] => Type::FullHouse,
        [1, 1, 3] => Type::Three,
        [1, 2, 2] => Type::TwoPair,
        [1, 1, 1, 2] => Type::OnePair,
        [1, 1, 1, 1, 1] => Type::HighCard,
        _ => panic!("Unexpected counts: {counts:?}"),
    }
}

// Hand type with no wild cards
fn hand_type(hand: Hand) -> Type {
    let mut counts: HashMap<Card, i32> = HashMap::new();
    for card in hand {
        *counts.entry(card).or_insert(0) += 1;
    }
    let mut counts_list: Vec<&i32> = counts.values().collect();
    counts_list.sort();
    type_counts(&counts_list)
}

// Hand type treating Jacks as Wild
fn hand_type_wild(hand: Hand) -> Type {
    let mut counts: HashMap<Card, i32> = HashMap::new();
    for card in hand {
        *counts.entry(card).or_insert(0) += 1;
    }
    if let Some(jacks) = counts.remove(&11) {
        let mut counts_list: Vec<&i32> = counts.values().collect();
        counts_list.sort();

        match jacks {
            1 => match counts_list[..] {
                [4] => Type::Five,
                [1, 3] => Type::Four,
                [2, 2] => Type::FullHouse,
                [1, 1, 2] => Type::Three,
                [1, 1, 1, 1] => Type::OnePair,
                _ => panic!("Unexpected counts 1J: {counts_list:?} {hand:?}"),
            },
            2 => match counts_list[..] {
                [3] => Type::Five,
                [1, 2] => Type::Four,
                [1, 1, 1] => Type::Three,
                _ => panic!("Unexpected counts 2J: {counts_list:?} {hand:?}"),
            },
            3 => match counts_list[..] {
                [2] => Type::Five,
                [1, 1] => Type::Four,
                _ => panic!("Unexpected counts 3J: {counts_list:?} {hand:?}"),
            },
            4 | 5 => Type::Five,
            _ => panic!("Bad number of jacks: {jacks} {hand:?}"),
        }
    } else {
        let mut counts_list: Vec<&i32> = counts.values().collect();
        counts_list.sort();
        type_counts(&counts_list)
    }
}

// Compare two hands (without wild cards)
fn cmp_hand(
    (a_hand, a_type, _): &(Hand, Type, u64),
    (b_hand, b_type, _): &(Hand, Type, u64),
) -> Ordering {
    a_type.cmp(b_type).then_with(|| a_hand.cmp(b_hand))
}

// Compare two hands with wild cards (and Jacks valued at 1)
fn cmp_hand_wild(
    (a_hand, a_type, _): &(Hand, Type, u64),
    (b_hand, b_type, _): &(Hand, Type, u64),
) -> Ordering {
    a_type.cmp(b_type).then_with(|| {
        let a_hand_wild: Vec<Card> = a_hand
            .iter()
            .map(|c| if *c == 11 { 1 } else { *c })
            .collect();
        let b_hand_wild: Vec<Card> = b_hand
            .iter()
            .map(|c| if *c == 11 { 1 } else { *c })
            .collect();
        a_hand_wild.cmp(&b_hand_wild)
    })
}

fn parse_game(s: &str) -> (Hand, u64) {
    let parts: Vec<&str> = s.split_ascii_whitespace().collect();
    if let [hand, bid] = parts[..] {
        (parse_hand(hand), bid.parse().unwrap())
    } else {
        panic!("Bad game")
    }
}

// Count winnings from sorted vector of games
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
        games.iter().map(|(h, i)| (*h, hand_type(*h), *i)).collect();
    games_no_wild.sort_by(cmp_hand);
    println!("{}", winnings(&games_no_wild));

    // part (b)
    let mut games_wild: Vec<(Hand, Type, u64)> = games
        .iter()
        .map(|(h, i)| (*h, hand_type_wild(*h), *i))
        .collect();
    games_wild.sort_by(cmp_hand_wild);
    println!("{}", winnings(&games_wild));
}
