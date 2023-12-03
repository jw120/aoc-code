// Advent of Code, 2022 day 6

use std::io;

fn find_header(s: &str, package_length: usize) -> usize {
    let datastream: Vec<char> = s.chars().collect();

    let mut letter_counts: [i32; 26] = [0; 26];
    let mut dupe_count = 0;
    let a_ord: u32 = 'a'.into();
    for i in 0..datastream.len() {
        let incoming: u32 = datastream[i].into();
        let incoming_index: usize = (incoming - a_ord) as usize;
        letter_counts[incoming_index] += 1;
        if letter_counts[incoming_index] > 1 {
            dupe_count += 1;
        }

        if i < package_length {
            continue;
        }

        let outgoing: u32 = datastream[i - package_length].into();
        let outgoing_index: usize = (outgoing - a_ord) as usize;
        letter_counts[outgoing_index] -= 1;
        if letter_counts[outgoing_index] > 0 {
            dupe_count -= 1;
        }

        if dupe_count == 0 {
            return i + 1;
        }
    }
    panic!("Not found");
}

fn main() {
    let mut input_string = String::new();
    io::stdin().read_line(&mut input_string).unwrap();

    println!("{}", find_header(&input_string, 4));
    println!("{}", find_header(&input_string, 14));
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_a() {
        assert_eq!(find_header("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4), 7);
        assert_eq!(find_header("bvwbjplbgvbhsrlpgdmjqwftvncz", 4), 5);
        assert_eq!(find_header("nppdvjthqldpwncqszvftbrmjlhg", 4), 6);
        assert_eq!(find_header("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4), 10);
        assert_eq!(find_header("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4), 11);
    }

    #[test]
    fn test_b() {
        assert_eq!(find_header("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14), 19);
        assert_eq!(find_header("bvwbjplbgvbhsrlpgdmjqwftvncz", 14), 23);
        assert_eq!(find_header("nppdvjthqldpwncqszvftbrmjlhg", 14), 23);
        assert_eq!(find_header("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14), 29);
        assert_eq!(find_header("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14), 26);
    }
}
