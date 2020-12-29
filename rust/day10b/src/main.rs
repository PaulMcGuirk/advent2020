use std::fs;
use std::collections::HashMap;

const FILEPATH: &str = "./input/input10.txt";

fn count_paths(adapters: &Vec<i32>) -> u64 {
    let mut adapters = adapters.clone();
    adapters.sort();

    let built_in = adapters.last().unwrap() + 3;
    adapters.push(built_in);

    let mut path_counts: HashMap<i32, u64> = HashMap::new();
    path_counts.insert(0, 1);

    for adapter in adapters {
        let path_count: u64 = (1..4)
            .map(|diff| path_counts.get(&(adapter - diff)).cloned().unwrap_or(0)).sum();
        path_counts.insert(adapter, path_count);
    }

    path_counts[&built_in]
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 10B");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let adapters: Result<Vec<_>, _> = contents.trim().lines().map(|line| line.parse::<i32>()).collect();

    let adapters = match adapters {
        Ok(adapters) => adapters,
        _ => {
            println!("Could not parse input");
            return
        }
    };

    let result = count_paths(&adapters);

    println!("{}", result);
}