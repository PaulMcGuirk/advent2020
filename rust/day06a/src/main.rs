use std::fs;
use std::collections::HashSet;

const FILEPATH: &str = "./input/input06.txt";

fn count_answers(group: &str) -> usize {
    let answers: HashSet<char> = group.replace("\n", "")
        .chars()
        .collect();
    answers.len()
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 06A");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let result: usize = contents.split("\n\n")
        .map(|group| count_answers(group))
        .sum();

    println!("{}", result);
}
