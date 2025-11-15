// Advent of Code 2024 - Day 9.

use std::io::{Read, stdin};

struct Chunk {
    id: Option<usize>,
    blocks: usize,
}

fn read_disk() -> Vec<Chunk> {
    let mut v: Vec<Chunk> = Vec::new();
    let mut buf = String::new();
    stdin().read_to_string(&mut buf).unwrap();
    for (i, ch) in buf.trim().chars().enumerate() {
        let blocks: usize = ch.to_digit(10).unwrap().try_into().unwrap();
        let id = if i % 2 == 0 { Some(i / 2) } else { None };
        v.push(Chunk { id, blocks });
    }
    v
}

fn compact(disk: &[Chunk]) -> usize {
    let total_file_blocks: usize = disk
        .iter()
        .filter(|chunk| chunk.id.is_some())
        .map(|chunk| chunk.blocks)
        .sum();

    let mut left_chunk: usize = 0;
    let mut right_chunk: usize = disk.len() - 1;

    let mut current_block: usize = 0;
    let mut check_sum: usize = 0;
    let mut left_summed: usize = 0;
    let mut right_transferred: usize = 0;

    assert!(disk[right_chunk].id.is_some());

    while current_block < total_file_blocks {
        assert!(disk[left_chunk].blocks > 0);
        assert!(disk[right_chunk].blocks > 0);
        if let Some(left_file_id) = disk[left_chunk].id {
            check_sum += left_file_id * current_block;
        } else {
            let right_file_id = disk[right_chunk].id.unwrap();
            check_sum += right_file_id * current_block;
            right_transferred += 1;
            if right_transferred >= disk[right_chunk].blocks {
                right_chunk -= 2;
                right_transferred = 0;
            }
        }
        left_summed += 1;
        while left_summed >= disk[left_chunk].blocks {
            left_chunk += 1;
            left_summed = 0;
        }
        current_block += 1;
    }
    check_sum
}

fn find_right_most_fit(
    current_chunk: usize,
    space: usize,
    disk: &[Chunk],
) -> Option<(usize, usize, usize)> {
    for i in (current_chunk..disk.len()).rev() {
        if let Some(i_id) = disk[i].id
            && disk[i].blocks <= space
        {
            return Some((i, i_id, disk[i].blocks));
        }
    }
    None
}

fn compact_whole(disk: &mut [Chunk]) -> usize {
    let mut current_chunk: usize = 0;
    let mut current_block: usize = 0;
    let mut check_sum: usize = 0;

    while current_chunk < disk.len() {
        let mut blocks = disk[current_chunk].blocks;
        if let Some(file_id) = disk[current_chunk].id {
            for _ in 0..blocks {
                check_sum += file_id * current_block;
                current_block += 1;
            }
        } else {
            while blocks > 0 {
                if let Some((r, r_id, r_blocks)) = find_right_most_fit(current_chunk, blocks, disk)
                {
                    for _ in 0..r_blocks {
                        check_sum += r_id * current_block;
                        current_block += 1;
                        blocks -= 1;
                    }
                    disk[r].id = None;
                } else {
                    current_block += blocks;
                    break;
                }
            }
        }
        current_chunk += 1;
    }
    check_sum
}

fn main() {
    let mut disk = read_disk();
    println!("{}", compact(&disk));
    println!("{}", compact_whole(&mut disk));
}
