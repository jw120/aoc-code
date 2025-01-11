// Advent of Code 2024 - Day 9.

use std::io::{stdin, Read};

struct Chunk {
    id: Option<usize>,
    blocks: usize,
    index: usize,
}

fn read_disk() -> Vec<Chunk> {
    let mut v: Vec<Chunk> = Vec::new();
    let mut index: usize = 0;
    let mut buf = String::new();
    stdin().read_to_string(&mut buf).unwrap();
    for (i, ch) in buf.trim().chars().enumerate() {
        let blocks: usize = ch.to_digit(10).unwrap().try_into().unwrap();
        let id = if i % 2 == 0 { Some(i / 2) } else { None };
        v.push(Chunk { id, blocks, index });
        index += blocks;
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

// def find_right_most_fit(
//     current_chunk: int,
//     space: int,
//     disk: list[Chunk],  # , used: list[bool]
// ) -> tuple[int, int, int] | None:
//     """Return index, id and blocks of right-most chunk whose size fits."""
//     for i in range(len(disk) - 1, current_chunk, -1):
//         # if not used[i]:
//         i_id = disk[i].id_
//         if i_id is not None and disk[i].blocks <= space:
//             return (i, i_id, disk[i].blocks)
//     return None

fn compact_whole(_disk: &[Chunk]) -> usize {
    0
}

// def compact_whole(disk: list[Chunk]) -> int:
//     """Compact the disk with whole files and return the checksum."""
//     current_chunk = 0
//     current_block = 0
//     check_sum = 0

//     while current_chunk < len(disk):
//         blocks = disk[current_chunk].blocks
//         match disk[current_chunk].id_:
//             case None:
//                 while blocks > 0:
//                     match find_right_most_fit(current_chunk, blocks, disk):  # , used):
//                         case None:
//                             current_block += blocks
//                             break
//                         case (r, r_id, r_blocks):
//                             for _ in range(r_blocks):
//                                 check_sum += r_id * current_block
//                                 current_block += 1
//                                 blocks -= 1
//                             disk[r].id_ = None  # Mark block as free space
//             case file_id:
//                 for _ in range(blocks):
//                     check_sum += file_id * current_block
//                     current_block += 1
//         current_chunk += 1
//     return check_sum

fn main() {
    let disk = read_disk();
    println!("{}", compact(&disk));
    println!("{}", compact_whole(&disk));
}
