// Advent of Code, 2024 day 17

use std::io;

use itertools::Itertools;

struct Device {
    a: u64,
    b: u64,
    c: u64,
    ip: usize,
    program: Vec<u8>,
}

impl Device {
    fn new(a: u64, b: u64, c: u64, program: &[u8]) -> Self {
        Self {
            a,
            b,
            c,
            ip: 0,
            program: Vec::from(program),
        }
    }

    fn reset(&mut self, a: u64) {
        self.a = a;
        self.b = 0;
        self.c = 0;
        self.ip = 0;
    }

    fn combo(&self, operand: u8) -> Option<u64> {
        match operand {
            0..=3 => Some(u64::from(operand)),
            4 => Some(self.a),
            5 => Some(self.b),
            6 => Some(self.c),
            _ => None,
        }
    }

    // Run until output or termination
    fn run_to_output(&mut self) -> Option<u8> {
        while self.ip < self.program.len() {
            if let Some(output) = self.step() {
                return Some(output);
            }
        }
        None
    }

    // Run until output or termination
    fn run_to_end(&mut self) -> Vec<u8> {
        let mut output: Vec<u8> = Vec::new();
        while self.ip < self.program.len() {
            if let Some(o) = self.step() {
                output.push(o);
            }
        }
        output
    }

    // Read and execute one instruction. Return output if any
    fn step(&mut self) -> Option<u8> {
        let opcode = self.program[self.ip];
        let literal_operand = self.program[self.ip + 1];
        let combo_operand = self.combo(literal_operand);
        self.ip += 2;
        match opcode {
            0 => self.a >>= combo_operand.unwrap(),
            1 => self.b ^= u64::from(literal_operand),
            2 => self.b = combo_operand.unwrap() & 7,
            3 => {
                if self.a != 0 {
                    self.ip = usize::from(literal_operand);
                }
            }
            4 => self.b ^= self.c,
            5 => return Some((combo_operand.unwrap() & 7) as u8),
            6 => self.b = self.a >> combo_operand.unwrap(),
            7 => self.c = self.a >> combo_operand.unwrap(),
            _ => panic!("Invalid 3-bit number for opcode: {opcode}"),
        }
        None
    }

    #[allow(dead_code)]
    fn disassemble(&self) {
        println!("a={}", self.a);
        println!("b={}", self.b);
        println!("c={}", self.c);
        println!("{:?}", self.program);
        for (i, o) in self.program.chunks(2).enumerate() {
            let opcode = o[0];
            let operand = o[1];
            let combo = match operand {
                0 => "0",
                1 => "1",
                2 => "2",
                3 => "3",
                4 => "a",
                5 => "b",
                6 => "c",
                _ => "Bad operand",
            };
            let ins = match opcode {
                0 => format!("a <- a >> {combo}"),
                1 => format!("b <- b ^ {operand}"),
                2 => format!("b <- {combo} & 7"),
                3 => format!("jump {operand} if a != 0"),
                4 => "b <- b ^ c".to_string(),
                5 => format!("output {combo} & 7"),
                6 => format!("b <- a >> {combo}"),
                7 => format!("c <- a >> {combo}"),
                _ => panic!("Bad opcode"),
            };
            println!("{:3}:  {ins}", i * 2);
        }
    }
}

// convert from series of 3-bit chunks with most significant first
fn from_3_rev(xs: &[u8]) -> u64 {
    let mut z: u64 = 0;
    for (i, x) in xs.iter().rev().enumerate() {
        z += u64::from(*x) << (3 * i);
    }
    z
}

fn part_a(device: &mut Device) -> String {
    let mut outputs: Vec<u8> = Vec::new();
    while let Some(output) = device.run_to_output() {
        outputs.push(output);
    }
    outputs.into_iter().join(",")
}

fn _part_b_brute_force(device: &mut Device, start: u64) -> u64 {
    let mut a: u64 = start;
    let program = device.program.clone();
    let mut best_match_count: usize = 0;
    'outer: loop {
        device.reset(a);
        let mut match_count: usize = 0;
        for expected in &program {
            match device.run_to_output() {
                Some(actual) if actual == *expected => {
                    match_count += 1;
                }
                _wrong => {
                    if match_count >= best_match_count {
                        best_match_count = match_count;
                    }
                    a += 1;
                    continue 'outer;
                }
            }
        }
        assert!(device.run_to_output().is_none());
        return a;
    }
}

fn part_b(device: &mut Device) -> u64 {
    let program = device.program.clone();
    let n = program.len();
    let mut answer: Vec<u8> = vec![0; n];
    for i in 0..n {
        'inner: for x in 0..=7 {
            if i == 0 && x == 0 {
                continue 'inner;
            }
            answer[i] = x;
            device.reset(from_3_rev(&answer));
            let output = device.run_to_end();
            assert_eq!(output.len(), n);
            if output[n - 1 - i..] == program[n - 1 - i..] {
                break 'inner;
            }
        }
    }
    from_3_rev(&answer)
}

fn read_reg(s: &str, x: Option<io::Result<String>>) -> u64 {
    x.unwrap()
        .unwrap()
        .strip_prefix(&format!("Register {s}: "))
        .unwrap()
        .parse()
        .unwrap()
}

fn read_prog(x: Option<io::Result<String>>) -> Vec<u8> {
    x.unwrap()
        .unwrap()
        .strip_prefix("Program: ")
        .unwrap()
        .split(',')
        .map(|s| s.parse::<u8>().unwrap())
        .collect()
}

fn main() {
    let mut iter = io::stdin().lines();
    let a: u64 = read_reg("A", iter.next());
    let b: u64 = read_reg("B", iter.next());
    let c: u64 = read_reg("C", iter.next());
    iter.next();
    let program: Vec<u8> = read_prog(iter.next());

    let mut device = Device::new(a, b, c, &program);
    // device.disassemble();

    println!("{}", part_a(&mut device));
    println!("{}", part_b(&mut device));
}
