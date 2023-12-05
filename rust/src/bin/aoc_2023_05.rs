// Advent of Code, 2023 day 05

use aoc_rust::stdin_lines;

#[derive(Debug)]
struct MapBlock {
    destination_range_start: u64,
    source_range_start: u64,
    range_length: u64,
}

impl MapBlock {
    fn new<S: AsRef<str>>(s: S) -> MapBlock {
        let mut space_iter = s.as_ref().trim().split_whitespace();
        let destination_range_start: u64 = space_iter.next().unwrap().parse().unwrap();
        let source_range_start: u64 = space_iter.next().unwrap().parse().unwrap();
        let range_length: u64 = space_iter.next().unwrap().parse().unwrap();
        assert!(space_iter.next().is_none());
        MapBlock {
            destination_range_start,
            source_range_start,
            range_length,
        }
    }
}

#[derive(Debug)]
struct Map {
    source: String,
    destination: String,
    blocks: Vec<MapBlock>,
}

impl Map {
    fn new(block: &[String]) -> Map {
        let mut header_iter = block[0].strip_suffix(" map:").unwrap().split("-to-");
        let source = header_iter.next().unwrap();
        let destination = header_iter.next().unwrap();
        assert!(header_iter.next().is_none());

        let mut blocks: Vec<MapBlock> = block[1..].iter().map(MapBlock::new).collect();
        blocks.sort_by_key(|b| b.source_range_start);

        Map {
            source: source.to_string(),
            destination: destination.to_string(),
            blocks,
        }
    }

    fn apply(&self, source: u64) -> u64 {
        for block in &self.blocks {
            if source < block.source_range_start {
                return source;
            }
            if source < block.source_range_start + block.range_length {
                return block.destination_range_start + (source - block.source_range_start);
            }
        }
        return source;
    }
}

fn apply_maps(maps: &[Map], source: u64) -> u64 {
    let mut x = source;
    for m in maps {
        x = m.apply(x);
    }
    x
}

fn parse_seeds(block: &[String]) -> Vec<u64> {
    assert!(block.len() == 1);
    block[0]
        .strip_prefix("seeds: ")
        .unwrap()
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect()
}

fn main() {
    let lines: Vec<String> = stdin_lines().collect();
    let mut blocks_iter = lines.split(|line| line.is_empty());

    let seeds = parse_seeds(blocks_iter.next().unwrap());
    let maps: Vec<Map> = blocks_iter.map(Map::new).collect();

    let locations: Vec<u64> = seeds.iter().map(|x| apply_maps(&maps, *x)).collect();

    // println!("{:?}", seeds);
    // println!("{:?}", maps);
    // println!("{:?}", locations);
    println!("{}", locations.iter().min().unwrap());
}
