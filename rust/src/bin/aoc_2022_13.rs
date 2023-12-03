// Advent of Code, 2022 day 13

use aoc_rust::stdin_lines;
use std::cmp::Ordering;

/// Chunks just iterate over the input strings (collecting digits into integers, dropping commas)
#[derive(Clone, Copy, Debug, PartialEq)]
enum Chunk {
    Open,
    Close,
    Number(u32),
}

struct ChunkIterator<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>, // peekable char iterator
    closes_due: u32,                                 // number of close parens expected
    // next two fields support promotion (e.g., of 7 into [7])
    next_chunk: Option<Chunk>, // next chunk to be returned (instead of following index)
    last: Option<u32>,         // previous value (from promotion)
}

impl<'a> ChunkIterator<'a> {
    fn new(s: &str) -> ChunkIterator {
        ChunkIterator {
            chars: s.chars().peekable(),
            last: None,
            next_chunk: None,
            closes_due: 0,
        }
    }
    fn promote(&mut self) {
        self.next_chunk = Some(Chunk::Number(self.last.unwrap()));
        self.closes_due += 1;
    }
}

impl<'a> Iterator for ChunkIterator<'a> {
    type Item = Chunk;

    fn next(&mut self) -> Option<Self::Item> {
        // deal with next_chunk if it has been set
        if let Some(next_chunk) = self.next_chunk {
            match next_chunk {
                Chunk::Close => {
                    if self.closes_due == 1 {
                        self.next_chunk = None;
                    } else {
                        self.closes_due -= 1;
                    }
                }
                Chunk::Number(_) => {
                    self.next_chunk = Some(Chunk::Close);
                }
                Chunk::Open => panic!("Unexpected"),
            }
            return Some(next_chunk);
        }

        let mut c = self.chars.next()?;

        // skip a comma
        if c == ',' {
            c = self.chars.next().unwrap();
        }

        // Handle open
        if c == '[' {
            self.last = None;
            return Some(Chunk::Open);
        }

        // Handle close
        if c == ']' {
            self.last = None;
            return Some(Chunk::Close);
        }

        // Handle numbers
        assert!(c.is_ascii_digit());
        let mut number_string: String = String::new();
        number_string.push(c);
        while let Some(cp) = self.chars.peek() {
            if cp.is_ascii_digit() {
                number_string.push(*cp);
                self.chars.next();
            } else {
                break;
            }
        }
        let number: u32 = number_string.parse().unwrap();
        self.last = Some(number);
        Some(Chunk::Number(number))
    }
}

fn compare(a: &str, b: &str) -> Ordering {
    let mut i = ChunkIterator::new(a);
    let mut j = ChunkIterator::new(b);
    let mut open_lists: u32 = 0;
    loop {
        match (i.next().unwrap(), j.next().unwrap()) {
            (Chunk::Open, Chunk::Open) => open_lists += 1,
            (Chunk::Open, Chunk::Close) => return Ordering::Greater,
            (Chunk::Open, Chunk::Number(_)) => j.promote(),
            (Chunk::Close, Chunk::Close) => {
                if open_lists == 1 {
                    return Ordering::Equal;
                } else {
                    open_lists += 1;
                }
            }
            (Chunk::Close, _) => return Ordering::Less,
            (Chunk::Number(_), Chunk::Open) => i.promote(),
            (Chunk::Number(_), Chunk::Close) => return Ordering::Greater,
            (Chunk::Number(p), Chunk::Number(q)) => {
                if p != q {
                    return p.cmp(&q);
                }
            }
        }
    }
}

fn sum_ordered(lines: &[String]) -> u32 {
    let mut pair: u32 = 1;
    let mut sum: u32 = 0;
    let mut lines_iter = lines.iter();
    while let Some(a) = lines_iter.next() {
        let b = lines_iter.next().unwrap();
        let blank = lines_iter.next();
        assert!(blank.map_or_else(|| true, |s| s.trim().is_empty()));
        if compare(a, b) != Ordering::Greater {
            sum += pair;
        }
        pair += 1;
    }
    sum
}

fn divider_positions(lines: &[String]) -> usize {
    // Take all non-blank packets
    let mut packets: Vec<&String> = lines.iter().filter(|s| !s.trim().is_empty()).collect();

    // Add two divider packets
    let divider1 = String::from("[[2]]");
    let divider2 = String::from("[[6]]");
    packets.push(&divider1);
    packets.push(&divider2);

    packets.sort_by(|x, y| compare(x, y));

    packets
        .iter()
        .enumerate()
        .filter(|(_, &s)| s == &divider1 || s == &divider2)
        .map(|(i, _)| i + 1)
        .product()
}

fn main() {
    let lines: Vec<String> = stdin_lines().collect();

    println!("{}", sum_ordered(&lines));
    println!("{}", divider_positions(&lines));
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_chunk() {
        let mut chunk_iter = ChunkIterator::new("[1,[234],56]");
        assert_eq!(chunk_iter.next(), Some(Chunk::Open));
        assert_eq!(chunk_iter.next(), Some(Chunk::Number(1)));
        assert_eq!(chunk_iter.next(), Some(Chunk::Open));
        assert_eq!(chunk_iter.next(), Some(Chunk::Number(234)));
        assert_eq!(chunk_iter.next(), Some(Chunk::Close));
        assert_eq!(chunk_iter.next(), Some(Chunk::Number(56)));
        assert_eq!(chunk_iter.next(), Some(Chunk::Close));
        assert_eq!(chunk_iter.next(), None);
    }

    #[test]
    fn test_compare() {
        assert_eq!(compare("[1,1,3,1,1]", "[1,1,5,1,1]"), Ordering::Less);
        assert_eq!(compare("[[1],[2,3,4]]", "[[1],4]"), Ordering::Less);
        assert_eq!(compare("[9]", "[[8,7,6]]"), Ordering::Greater);
        assert_eq!(compare("[[4,4],4,4]", "[[4,4],4,4,4]"), Ordering::Less);
        assert_eq!(compare("[7,7,7,7]", "[7,7,7]"), Ordering::Greater);
        assert_eq!(compare("[]", "[3]"), Ordering::Less);
        assert_eq!(compare("[[[]]]", "[[]]"), Ordering::Greater);
        assert_eq!(
            compare("[1,[2,[3,[4,[5,6,7]]]],8,9]", "[1,[2,[3,[4,[5,6,0]]]],8,9]"),
            Ordering::Greater
        );
        assert_eq!(
            compare(
                "[[8,[[2,6,0,9],[4,9,5,5,3],[8],8,3],[1]],[0,[7,[8,8,8,1]],[],[[0],[8,6,9,9,1],2,6,2]],[],[1,[2,1,[1],8],[],8],[[9,7],[7,10],[[],9,9,7,3],[2,[],6],[[],5,[7,6,7,6,0],[3],[3,10,10,10]]]]", 
                "[[[[8],1,[1,1,3,5,1],[0,3]],[],4,0,5],[],[3,3,[9,4,5,[4,2,3]],7,9],[7],[[[2,1]],[[2,9,10,0,6],[3,0,8],[0,3,2,9,1]]]]"
            ),
            Ordering::Less
        );
    }
}
