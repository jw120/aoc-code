// Advent of Code, 2022 day 15

use aoc_rust::stdin_lines;
use aoc_rust::Coord;
use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::zip;

#[derive(Clone, Copy, Debug, PartialEq)]
struct Report {
    sensor: Coord,
    beacon: Coord,
}

fn parse(line: &str) -> Report {
    let parts: Vec<&str> = line.split('=').collect();
    assert_eq!(parts.len(), 5);
    assert_eq!(parts[0], "Sensor at x");
    let numbers: Vec<i32> = parts[1..]
        .iter()
        .map(|s| s.trim_end_matches(|c: char| !c.is_numeric()))
        .map(|s| s.parse().unwrap())
        .collect();
    Report {
        sensor: Coord {
            row: numbers[1],
            col: numbers[0],
        },
        beacon: Coord {
            row: numbers[3],
            col: numbers[2],
        },
    }
}

// Test if the given cell must be empty.
fn must_be_empty(reports: &[Report], test_beacon: Coord) -> bool {
    for r in reports {
        if r.beacon == test_beacon {
            return false;
        }
        let closest_beacon_distance = r.sensor.manhattan(&r.beacon);
        let test_distance = r.sensor.manhattan(&test_beacon);
        if test_distance <= closest_beacon_distance {
            return true;
        }
    }
    false
}

// Return number of positions with given y coordinate that must be empty.
fn empty_in_row(reports: &[Report], y: i32) -> usize {
    let offsets: Vec<Coord> = reports
        .iter()
        .map(|r| Coord {
            row: 0,
            col: r.sensor.manhattan(&r.beacon),
        })
        .collect();
    let x_min: i32 = zip(reports, &offsets)
        .map(|(r, o)| (r.sensor - *o).col)
        .min()
        .unwrap();
    let x_max: i32 = zip(reports, &offsets)
        .map(|(r, o)| (r.sensor + *o).col)
        .max()
        .unwrap();
    (x_min..x_max + 1)
        .filter(|x| must_be_empty(reports, Coord { row: y, col: *x }))
        .count()
}

fn get_constraint(sensor1: Coord, sensor2: Coord, radius1: i32, radius2: i32) -> (bool, i32) {
    assert!(sensor1.row != sensor2.row && sensor1.col != sensor2.col);
    let (s1, s2, r1, r2) = if sensor1.col <= sensor2.col {
        (sensor1, sensor2, radius1, radius2)
    } else {
        (sensor2, sensor1, radius2, radius1)
    };
    assert!(s1.col < s2.col);
    if s1.row < s2.row {
        assert!(s1.col + s1.row + r1 + 1 == s2.col + s2.row - r2 - 1);
        return (true, s1.col + s1.row + r1 + 1);
    }
    assert!(s1.col - s1.row + r1 + 1 == s2.col - s2.row - r2 - 1);
    (false, s1.col - s1.row + r1 + 1)
}

fn find_beacon(reports: &[Report]) -> Coord {
    let sensor_radius: HashMap<Coord, i32> = reports
        .iter()
        .map(|r| (r.sensor, r.sensor.manhattan(&r.beacon)))
        .collect();
    let mut x_plus_y_constraints: HashSet<i32> = HashSet::new();
    let mut x_minus_y_constraints: HashSet<i32> = HashSet::new();
    for report1 in reports {
        for report2 in reports {
            if report1 == report2 {
                continue;
            }
            let s1: Coord = report1.sensor;
            let s2: Coord = report2.sensor;
            let d: i32 = s1.manhattan(&s2);
            let r1: i32 = *sensor_radius.get(&s1).unwrap();
            let r2: i32 = *sensor_radius.get(&s2).unwrap();
            if d == (r1 + 1) + (r2 + 1) {
                let (constraint_sign, constraint_value) = get_constraint(s1, s2, r1, r2);
                if constraint_sign {
                    x_plus_y_constraints.insert(constraint_value);
                } else {
                    x_minus_y_constraints.insert(constraint_value);
                }
            }
        }
    }
    assert!(!x_plus_y_constraints.is_empty() && !x_minus_y_constraints.is_empty());
    let mut solutions: HashSet<Coord> = HashSet::new();
    for p_con in x_plus_y_constraints.iter() {
        for m_con in x_minus_y_constraints.iter() {
            if p_con + m_con % 2 == 1 || p_con - m_con % 2 == 1 {
                continue;
            }
            let col = (p_con + m_con) / 2;
            let row = (p_con - m_con) / 2;
            solutions.insert(Coord { row, col });
        }
    }
    for solution in solutions {
        if reports
            .iter()
            .all(|r| solution.manhattan(&r.sensor) > *sensor_radius.get(&r.sensor).unwrap())
        {
            return solution;
        }
    }
    unreachable!();
}

fn main() {
    let reports: Vec<Report> = stdin_lines().map(|s| parse(&s)).collect();

    println!("{}", empty_in_row(&reports, 2_000_000));
    let beacon = find_beacon(&reports);
    let encoded: i64 = (beacon.col as i64) * 4_000_000 + (beacon.row as i64);
    println!("{}", encoded);
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse() {
        let line = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15";
        assert_eq!(
            parse(line),
            Report {
                sensor: Coord { row: 18, col: 2 },
                beacon: Coord { row: 15, col: -2 }
            }
        );
    }
}
