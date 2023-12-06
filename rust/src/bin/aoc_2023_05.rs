// Advent of Code, 2023 day 05

use aoc_rust::stdin_lines;
use std::cmp::max;

#[derive(Clone, Copy, Debug, PartialEq)]
struct Range {
    lo: i64,
    hi: i64,
}

fn range(lo: i64, hi: i64) -> Range {
    assert!(lo <= hi);
    assert!(lo >= 0);
    Range { lo, hi }
}

impl Range {
    fn width(&self) -> i64 {
        assert!(self.lo <= self.hi);
        self.hi - self.lo + 1
    }
}

#[derive(Debug)]
struct MapBlock {
    source: Range,
    destination: Range,
}

impl MapBlock {
    fn new<S: AsRef<str>>(s: S) -> MapBlock {
        let mut space_iter = s.as_ref().trim().split_whitespace();
        let destination_range_start: i64 = space_iter.next().unwrap().parse().unwrap();
        let source_range_start: i64 = space_iter.next().unwrap().parse().unwrap();
        let range_length: i64 = space_iter.next().unwrap().parse().unwrap();
        assert!(space_iter.next().is_none());
        MapBlock {
            source: Range {
                lo: source_range_start,
                hi: source_range_start + range_length - 1,
            },
            destination: Range {
                lo: destination_range_start,
                hi: destination_range_start + range_length - 1,
            },
        }
    }

    fn _apply(&self, input: i64) -> i64 {
        if input >= self.source.lo && input <= self.source.hi {
            input + self.destination.lo - self.source.lo
        } else {
            input
        }
    }

    fn _apply_range(&self, input: Range) -> Vec<Range> {
        let shift = self.destination.lo - self.source.lo;
        if input.lo < self.source.lo {
            if input.hi < self.source.lo {
                vec![input]
            } else if input.hi <= self.source.hi {
                vec![
                    range(input.lo, self.source.lo - 1),
                    range(self.destination.lo, input.hi + shift),
                ]
            } else {
                vec![
                    range(input.lo, self.source.lo - 1),
                    self.destination,
                    range(self.source.hi + 1, input.hi),
                ]
            }
        } else if input.lo <= self.source.hi {
            if input.hi <= self.source.hi {
                vec![range(input.lo + shift, input.hi + shift)]
            } else {
                vec![
                    range(input.lo + shift, self.destination.hi),
                    range(self.source.hi + 1, input.hi),
                ]
            }
        } else {
            vec![input]
        }
    }
}

#[derive(Debug)]
struct Map {
    _source: String,
    _destination: String,
    blocks: Vec<MapBlock>,
}

impl Map {
    fn new(block: &[String]) -> Map {
        let mut header_iter = block[0].strip_suffix(" map:").unwrap().split("-to-");
        let source = header_iter.next().unwrap();
        let destination = header_iter.next().unwrap();
        assert!(header_iter.next().is_none());

        let mut blocks: Vec<MapBlock> = block[1..].iter().map(MapBlock::new).collect();
        blocks.sort_by_key(|b| b.source.lo);

        Map {
            _source: source.to_string(),
            _destination: destination.to_string(),
            blocks,
        }
    }

    fn apply(&self, source: i64) -> i64 {
        for block in &self.blocks {
            if source < block.source.lo {
                return source;
            }
            if source <= block.source.hi {
                return source + block.destination.lo - block.source.lo;
            }
        }
        return source;
    }

    fn apply_range(&self, source: Range) -> Vec<Range> {
        let mut lo: i64 = source.lo;
        let hi: i64 = source.hi;
        let mut out: Vec<Range> = Vec::new();
        for block in &self.blocks {
            let shift = block.destination.lo - block.source.lo;
            if lo < block.source.lo {
                if hi < block.source.lo {
                    out.push(range(lo, hi));
                    break;
                } else if hi < block.source.hi {
                    out.push(range(lo, block.source.lo - 1));
                    out.push(range(block.destination.lo, hi + shift));
                    break;
                } else {
                    out.push(range(lo, block.source.lo - 1));
                    out.push(block.destination);
                    lo = block.source.hi + 1;
                }
            } else if lo <= block.source.hi {
                if hi <= block.source.hi {
                    out.push(range(lo + shift, hi + shift));
                    break;
                } else {
                    out.push(range(lo + shift, block.destination.hi));
                    lo = block.source.hi + 1;
                }
            } else {
                // do nothing - just skip to next block
            }
        }
        let last_hi = self.blocks.last().unwrap().source.hi;
        if hi > last_hi {
            out.push(range(max(lo, last_hi + 1), hi));
        }
        assert_eq!(source.width(), out.iter().map(|r| r.width()).sum());
        out
    }

    fn apply_ranges(&self, source: &[Range]) -> Vec<Range> {
        source.iter().flat_map(|r| self.apply_range(*r)).collect()
    }
}

fn apply_maps(maps: &[Map], source: i64) -> i64 {
    let mut x = source;
    for m in maps {
        x = m.apply(x);
    }
    x
}

fn apply_maps_range(maps: &[Map], source: &[Range]) -> Vec<Range> {
    let mut rs: Vec<Range> = Vec::from(source);
    for m in maps {
        rs = m.apply_ranges(&rs);
    }
    rs
}

fn parse_seeds(block: &[String]) -> Vec<i64> {
    assert!(block.len() == 1);
    block[0]
        .strip_prefix("seeds: ")
        .unwrap()
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect()
}

fn parse_seed_ranges(block: &[String]) -> Vec<Range> {
    parse_seeds(block)
        .chunks(2)
        .map(|chunk| range(chunk[0], chunk[0] + chunk[1] - 1))
        .collect()
}

fn main() {
    let lines: Vec<String> = stdin_lines().collect();
    let mut blocks_iter = lines.split(|line| line.is_empty());
    let seeds_block = blocks_iter.next().unwrap();
    let maps: Vec<Map> = blocks_iter.map(Map::new).collect();

    // part (a)
    let seeds = parse_seeds(seeds_block);
    let locations: Vec<i64> = seeds.iter().map(|x| apply_maps(&maps, *x)).collect();
    println!("{}", locations.iter().min().unwrap());

    // part (b)
    let seed_ranges: Vec<Range> = parse_seed_ranges(seeds_block);
    let location_ranges: Vec<Range> = apply_maps_range(&maps, &seed_ranges);
    println!("{}", location_ranges.iter().map(|r| r.lo).min().unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_block_apply() {
        let b = MapBlock {
            source: Range { lo: 10, hi: 20 },
            destination: Range { lo: 110, hi: 120 },
        };
        // 3 cases: input below, within or above the block
        assert_eq!(b._apply(5), 5);
        assert_eq!(b._apply(15), 115);
        assert_eq!(b._apply(25), 25);
    }

    #[test]
    fn test_block_apply_range() {
        let b = MapBlock {
            source: Range { lo: 10, hi: 20 },
            destination: Range { lo: 110, hi: 120 },
        };
        // 3 cases with lo below block range
        assert_eq!(b._apply_range(range(5, 6)), vec![range(5, 6)]);
        assert_eq!(
            b._apply_range(range(5, 15)),
            vec![range(5, 9), range(110, 115)]
        );
        assert_eq!(
            b._apply_range(range(5, 25)),
            vec![range(5, 9), range(110, 120), range(21, 25)]
        );
        // 2 cases with lo within block range
        assert_eq!(b._apply_range(range(15, 17)), vec![range(115, 117)]);
        assert_eq!(
            b._apply_range(range(15, 24)),
            vec![range(115, 120), range(21, 24)]
        );
        // 1 cases with lo above block range
        assert_eq!(b._apply_range(range(25, 28)), vec![range(25, 28)]);
    }

    #[test]
    fn test_map_apply_range() {
        let m: Map = Map {
            _source: "s".to_string(),
            _destination: "d".to_string(),
            blocks: vec![
                MapBlock {
                    source: Range { lo: 10, hi: 20 },
                    destination: Range { lo: 50, hi: 60 },
                },
                MapBlock {
                    source: Range { lo: 40, hi: 80 },
                    destination: Range { lo: 140, hi: 180 },
                },
            ],
        };

        assert_eq!(
            m.apply_range(range(0, 100)),
            vec![
                range(0, 9),
                range(50, 60),
                range(21, 39),
                range(140, 180),
                range(81, 100)
            ]
        );
    }
}
