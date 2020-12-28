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

fn find_seq(nums: &Vec<i64>, target: i64) -> Option<Vec<i64>> {
    for i in 0..nums.len() {
        let mut sum = nums[i];
        for j in (i + 1)..nums.len() {
            sum += nums[j];
            if sum == target {
                return Some(nums[i..j].to_vec());
            }
        }
    }
    None
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 09B");

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

    let invalid = match find_invalid(&nums, 25) {
        Some(invalid) => invalid,
        _ => {
            println!("No result found");
            return;
        }
    };

    let seq = match find_seq(&nums, invalid) {
        Some(seq) => seq,
        _ => {
            println!("No result found");
            return;
        }
    };

    let result = seq.iter().min().unwrap() + seq.iter().max().unwrap();
    println!("{}", result);
}