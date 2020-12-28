use std::fs;
use std::collections::HashSet;

const FILEPATH: &str = "./input/input09.txt";

fn find_invalid(nums: &Vec<i64>, preamble_size: usize) -> Option<i64> {
    let mut group: HashSet<i64> = nums[0..preamble_size].iter().cloned().collect();

    for (i, &num) in nums.iter().skip(preamble_size).enumerate() {
        if !group.iter().any(|&a| {2 * a != num && group.contains(&(num - a)) }) {
            return Some(num)
        }
        group.remove(&nums[i]);
        group.insert(num);
    }

    None
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 09A");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let nums: Result<Vec<_>, _> = contents.trim().lines().map(|line| line.parse::<i64>()).collect();

    let nums = match nums {
        Ok(nums) => nums,
        _ => {
            println!("Could not parse input");
            return
        }
    };

    let result = match find_invalid(&nums, 25) {
        Some(result) => result,
        _ => {
            println!("No result found");
            return;
        }
    };

    println!("{}", result);
}