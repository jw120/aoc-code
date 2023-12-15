// Advent of Code, 2023 day XX

use std::io::{stdin, Read};
use std::iter::repeat_with;

fn hash(s: &str) -> u8 {
    let mut acc: u8 = 0;
    for ch in s.chars() {
        acc = acc.wrapping_add(ch as u8);
        acc = acc.wrapping_mul(17);
    }
    acc
}

fn hash_seq(s: &str) -> u32 {
    s.split(',').map(|x| hash(x) as u32).sum()
}

fn run(s: &str) -> usize {
    let mut bxes: Vec<Vec<(&str, u8)>> = repeat_with(Vec::new).take(256).collect();
    for step in s.split(',') {
        let operation_index: usize = step.find(['-', '=']).unwrap();
        let step_label: &str = &step[0..operation_index];
        let step_bx: &mut Vec<(&str, u8)> = &mut bxes[hash(step_label) as usize];

        if step.chars().nth(operation_index).unwrap() == '-' {
            // delete lens
            for (lens_index, (lens_label, _lens_length)) in step_bx.iter().enumerate() {
                if step_label == *lens_label {
                    step_bx.remove(lens_index);
                    break;
                }
            }
        } else {
            // replace or insert lens
            let step_length: u8 = step[operation_index + 1..].parse().unwrap();
            let mut found: bool = false;
            for (lens_index, (lens_label, _lens_length)) in step_bx.iter().enumerate() {
                if step_label == *lens_label {
                    step_bx[lens_index] = (step_label, step_length);
                    found = true;
                    break;
                }
            }
            if !found {
                step_bx.push((step_label, step_length));
            }
        }
    }

    let mut acc: usize = 0;
    for (bx_index, bx) in bxes.iter().enumerate() {
        for (lens_index, (_lens_label, lens_length)) in bx.iter().enumerate() {
            acc += (bx_index + 1) * (lens_index + 1) * (*lens_length as usize);
        }
    }
    acc
}

fn main() {
    let mut problem = String::new();
    stdin().read_to_string(&mut problem).unwrap();

    println!("{}", hash_seq(problem.trim()));
    println!("{}", run(problem.trim()));
}
