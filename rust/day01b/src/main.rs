use std::fs;

const FILEPATH: &str = "./input/input01.txt";

fn parse_contents(contents: &str) -> Result<Vec<u32>, std::num::ParseIntError> {
    contents.lines()
        .map(|line| line.parse::<u32>())
        .collect()
}

fn find_subset(entries: &Vec<u32>, target: u32, subset_size: usize) -> Option<Vec<u32>> {
    fn find_subset_helper(
        entries: &Vec<u32>,
        target: u32,
        subset_size: usize,
        working: &Vec<u32>,
        index: usize) -> Option<Vec<u32>> {

            if working.len() == subset_size {
                return if working.iter().sum::<u32>() == target {
                    Some(working.clone())
                } else {
                    None
                };
            }

            if index >= entries.len() {
                return None;
            }

            match find_subset_helper(entries, target, subset_size, working, index + 1) {
                Some(result) => Some(result),
                None => {
                    let mut working = working.clone();
                    working.push(entries[index]);
                    find_subset_helper(entries, target, subset_size, &working, index + 1)
                }
            }
    }
    find_subset_helper(entries, target, subset_size, &Vec::new(), 0)
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 01B");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let entries = match parse_contents(&contents) {
        Ok(entries) => entries,
        _ => {
            println!("Couldn't parse file");
            return;
        }
    };

    let subset = match find_subset(&entries, 2020, 3) {
        Some(subset) => subset,
        _ => {
            println!("No solution found");
            return;
        }
    };

    let result = subset.iter().fold(1, |prod, val| prod * val);
    println!("{}", result)
}
