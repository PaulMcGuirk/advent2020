use std::fs;

const FILEPATH: &str = "./input/input01.txt";

fn parse_contents(contents: &str) -> Result<Vec<u32>, std::num::ParseIntError> {
    contents.lines()
        .map(|line| line.parse::<u32>())
        .collect()
}

fn find_pair(entries: &Vec<u32>, target: u32) -> Option<(u32, u32)> {
    for i in 0..entries.len() {
        let a = entries[i];
        for j in (i + 1)..entries.len() {
            let b = entries[j];
            if a + b == target {
                return Some((a, b))
            }
        }
    }
    None
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 01A");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let entries = match parse_contents(&contents) {
        Ok(entries) => entries,
        _ => {
            println!("Couldn't parse file");
            return;
        }
    };

    let (a, b) = match find_pair(&entries, 2020){
        Some(pair) => pair,
        _ => {
            println!("No result found");
            return;
        }
    };

    println!("{}", a * b);
}
