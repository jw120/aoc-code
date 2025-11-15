// Advent of Code, 2022 day 7

use aoc_rust::stdin_lines;
use std::collections::HashMap;

// Directories map strings to files or folders
type Dir = HashMap<String, DirItem>;

// Filesystem tree of directories held in a vector, so pointers are indices
type DirPtr = usize;

enum DirItem {
    File(usize),
    Folder(DirPtr),
}

// Hold all of our file system
struct FileSystem {
    dirs: Vec<Dir>,
    current_dir: DirPtr,
}

impl FileSystem {
    fn new() -> FileSystem {
        let mut fs = FileSystem {
            dirs: Vec::new(),
            current_dir: 0,
        };
        let root_dir = fs.new_dir();
        assert_eq!(root_dir, 0);
        fs
    }

    // Insert into current directory
    fn insert(&mut self, name: &str, item: DirItem) {
        self.dirs[self.current_dir].insert(String::from(name), item);
    }

    // Create a new directory under current dir
    fn new_dir(&mut self) -> DirPtr {
        let mut new_dir = HashMap::new();
        new_dir.insert(String::from(".."), DirItem::Folder(self.current_dir));
        new_dir.insert(String::from("/"), DirItem::Folder(0));
        self.dirs.push(new_dir);
        self.dirs.len() - 1
    }

    // Add an entry to current directory with a new sub-director
    fn add_sub_dir(&mut self, name: &str) -> DirPtr {
        let new_dir_ptr = self.new_dir();
        self.dirs[self.current_dir].insert(String::from(name), DirItem::Folder(new_dir_ptr));
        new_dir_ptr
    }

    fn process_line(&mut self, line: &str) {
        let mut words_iter = line.split_whitespace();
        let word1 = words_iter.next().unwrap();
        if word1 == "$" {
            match words_iter.next().unwrap() {
                "ls" => {
                    assert!(words_iter.next().is_none());
                    // do nothing
                }
                "cd" => {
                    let arg = words_iter.next().unwrap();
                    match self.dirs[self.current_dir].get(arg) {
                        Some(DirItem::File(_)) => panic!("Can't cd to file"),
                        Some(DirItem::Folder(d)) => self.current_dir = *d,
                        None => self.current_dir = self.add_sub_dir(arg),
                    }
                }
                &_ => panic!("Unknown command"),
            }
        } else {
            let word2 = words_iter.next().unwrap();
            if word1 == "dir" {
                let new_dir = self.add_sub_dir(word2);
                self.insert(word2, DirItem::Folder(new_dir));
            } else {
                self.insert(word2, DirItem::File(word1.parse().unwrap()));
            }
        }
    }

    // return sizes of all directories
    fn get_dir_sizes(&self) -> Vec<usize> {
        let mut sizes = Vec::new();
        self.add_dir_sizes(0, &mut sizes);
        sizes
    }

    fn add_dir_sizes(&self, d: DirPtr, sizes: &mut Vec<usize>) -> usize {
        let mut size: usize = 0;
        for (name, entry) in &self.dirs[d] {
            if name != ".." && name != "/" {
                // println!("{}", name);
                match entry {
                    DirItem::File(file_size) => size += file_size,
                    DirItem::Folder(sub_dir) => size += self.add_dir_sizes(*sub_dir, sizes),
                }
            }
        }
        sizes.push(size);
        size
    }
}

fn main() {
    let mut fs = FileSystem::new();
    for line in stdin_lines() {
        fs.process_line(&line);
    }
    let sizes = fs.get_dir_sizes();

    let part_a: usize = sizes.iter().filter(|x| **x <= 100_000).sum();
    println!("{part_a}");

    let current_space = 70_000_000 - sizes[sizes.len() - 1];
    let space_needed = 30_000_000 - current_space;
    let part_b = sizes.iter().filter(|x| **x >= space_needed).min().unwrap();
    println!("{part_b}");
}
