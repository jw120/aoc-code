// Advent of Code, 2022 day XX

use aoc_rust::stdin_lines;

enum Instruction {
    Addx(i32),
    Noop,
}

impl Instruction {
    fn parse(line: &str) -> Instruction {
        let mut words_iter = line.split_whitespace();
        let ins = match words_iter.next().unwrap() {
            "noop" => Instruction::Noop,
            "addx" => Instruction::Addx(words_iter.next().unwrap().parse().unwrap()),
            _ => panic!("Unrecognised instruction"),
        };
        assert!(words_iter.next().is_none());
        ins
    }

    fn cycles_needed(&self) -> i32 {
        match self {
            Instruction::Noop => 1,
            Instruction::Addx(_) => 2,
        }
    }

    fn delta_x(&self) -> i32 {
        match self {
            Instruction::Noop => 0,
            Instruction::Addx(dx) => *dx,
        }
    }
}

fn run(instructions: &[Instruction]) -> (i32, Vec<char>) {
    let mut cycle: i32 = 1;
    let mut signal_strength: i32 = 0;
    let mut x: i32 = 1;
    let mut crt: Vec<char> = Vec::new();

    for ins in instructions {
        for _ in 0..ins.cycles_needed() {
            if (cycle - 20) % 40 == 0 {
                signal_strength += cycle * x;
            }
            let crt_position: i32 = (cycle - 1) % 40;
            let crt_diff: i32 = (crt_position - x).abs();
            crt.push(if crt_diff <= 1 { '#' } else { '.' });
            cycle += 1;
        }
        x += ins.delta_x();
    }
    (signal_strength, crt)
}

fn main() {
    let instructions: Vec<Instruction> = stdin_lines().map(|s| Instruction::parse(&s)).collect();
    let (signal, crt) = run(&instructions);
    println!("{}", signal);
    for (i, c) in crt.iter().enumerate() {
        print!("{}", c);
        if (i + 1) % 40 == 0 {
            println!();
        }
    }
}
