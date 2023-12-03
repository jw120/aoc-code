// Advent of Code, 2022 day XX

use aoc_rust::stdin_lines;

type Stack = Vec<char>;

fn parse_stacks(ss: &[String]) -> Vec<Stack> {
    // We read lines backwards
    let mut iter = ss.iter().rev();

    // Use last row to set up n empty vectors
    let number_row = iter.next().unwrap();
    let n = number_row.split_whitespace().count();
    let mut v: Vec<Stack> = Vec::new();
    for _ in 0..n {
        v.push(Vec::new());
    }

    // Add to stacks
    for row in iter {
        for (stack_index, stack_vec) in v.iter_mut().enumerate() {
            let c = row.chars().nth(stack_index * 4 + 1).unwrap();
            if c != ' ' {
                stack_vec.push(c);
            }
        }
    }
    v
}

#[derive(Debug)]
struct Move {
    quantity: usize,
    from: usize, // 1-based
    to: usize,   // 1-based
}

impl Move {
    fn parse(s: &str) -> Move {
        let mut iter = s.trim().split(' ');
        assert_eq!(iter.next(), Some("move"));
        let quantity: usize = iter.next().unwrap().parse().unwrap();
        assert_eq!(iter.next(), Some("from"));
        let from: usize = iter.next().unwrap().parse().unwrap();
        assert_eq!(iter.next(), Some("to"));
        let to: usize = iter.next().unwrap().parse().unwrap();
        assert!(iter.next().is_none());

        Move { quantity, from, to }
    }
}

fn apply_moves_a(stacks: &mut [Stack], moves: &[Move]) {
    for m in moves {
        for _ in 0..m.quantity {
            let c = stacks[m.from - 1].pop().unwrap();
            stacks[m.to - 1].push(c);
        }
    }
}

fn apply_moves_b(stacks: &mut [Stack], moves: &[Move]) {
    for m in moves {
        let from_len = stacks[m.from - 1].len();
        for i in 0..m.quantity {
            let c = stacks[m.from - 1][from_len - m.quantity + i];
            stacks[m.to - 1].push(c);
        }
        stacks[m.from - 1].truncate(from_len - m.quantity);
    }
}

fn print_stack_tops(stacks: &[Stack]) {
    for stack in stacks {
        print!("{}", stack[stack.len() - 1]);
    }
    println!();
}

fn main() {
    let stacks_lines: Vec<String> = stdin_lines().take_while(|s| !s.trim().is_empty()).collect();
    let mut stacks_a: Vec<Stack> = parse_stacks(stacks_lines.as_slice());
    let mut stacks_b: Vec<Stack> = stacks_a.to_vec();

    let moves: Vec<Move> = stdin_lines().map(|s| Move::parse(&s)).collect();

    apply_moves_a(stacks_a.as_mut_slice(), moves.as_slice());
    print_stack_tops(stacks_a.as_slice());

    apply_moves_b(stacks_b.as_mut_slice(), moves.as_slice());
    print_stack_tops(stacks_b.as_slice());
}
