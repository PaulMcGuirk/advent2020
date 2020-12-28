use std::fs;
use std::collections::HashMap;

const FILEPATH: &str = "./input/input10.txt";

fn get_differences(adapters: &Vec<i32>) -> HashMap<i32, usize> {
    let mut adapters = adapters.clone();
    adapters.sort();

    let mut counts = HashMap::new();
    counts.insert(adapters[0], 1);

    for i in 1..adapters.len() {
        let diff = adapters[i] - adapters[i - 1];
        *counts.entry(diff).or_default() += 1;
    }

    *counts.entry(3).or_default() += 1; // handle the last adapeter

    counts
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 10A");

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

    let differences = get_differences(&adapters);
    
    let result = *differences.get(&3).unwrap_or(&0) * *differences.get(&1).unwrap_or(&0);

    println!("{}", result);
}