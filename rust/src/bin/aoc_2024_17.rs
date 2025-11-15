// Advent of Code, 2024 day 17

use std::io;

struct Device {
    a: u64,
    b: u64,
    c: u64,
    ip: usize,
    output: Vec<u8>,
    program: Vec<u8>,
}

impl Device {
    fn new(a: u64, b: u64, c: u64, program: &[u8]) -> Self {
        Self {
            a,
            b,
            c,
            ip: 0,
            output: Vec::new(),
            program: Vec::from(program),
        }
    }

    fn combo(&self, operand: u8) -> u64 {
        match operand {
            0..=3 => u64::from(operand),
            4 => self.a,
            5 => self.b,
            6 => self.c,
            7 => panic!("Combo operand 7 not expected"),
            _ => panic!("Invalid 3-bit number for combo operand: {operand}"),
        }
    }

    // Read and execute one instruction. Return true if finished
    fn step(&mut self) -> bool {
        println!("{}: a={} b={} c={}", self.ip, self.a, self.b, self.c);
        let Some(opcode) = self.program.get(self.ip) else {
            return true;
        };
        let Some(operand) = self.program.get(self.ip + 1) else {
            panic!("No operand");
        };
        self.ip += 2;
        match opcode {
            0 => self.a >>= self.combo(*operand),
            1 => self.b ^= u64::from(*operand),
            2 => self.b = self.combo(*operand) & 7,
            3 => {
                if self.a != 0 {
                    self.ip = usize::from(*operand);
                }
            }
            4 => self.b ^= self.c,
            5 => self.output.push((self.combo(*operand) & 7) as u8),
            6 => self.b = self.a >> self.combo(*operand),
            7 => self.c = self.a >> self.combo(*operand),
            _ => panic!("Invalid 3-bit number for opcode: {opcode}"),
        }
        false
    }

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
                3 => format!("  jump {operand} if a != 0"),
                4 => "b <- b ^ c".to_string(),
                5 => format!("  output {combo} & 7"),
                6 => format!("b <- a >> {combo}"),
                7 => format!("c <- a >> {combo}"),
                _ => panic!("Bad opcode"),
            };
            println!("{:3}:  {ins}", i * 2);
        }
    }
}

fn part_a(a: u64, b: u64, c: u64, program: &[u8]) -> String {
    let mut device = Device::new(a, b, c, program);
    device.disassemble();
    while !device.step() {}
    format!("{:?}", device.output)
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

    println!("{}", part_a(a, b, c, &program));
}
