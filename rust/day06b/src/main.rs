use std::fs;
use std::collections::HashSet;

const FILEPATH: &str = "./input/input06.txt";

fn count_answers(group: &str) -> usize {
    let mut answers = group.lines()
        .map(|line| line.chars().collect::<HashSet<_>>());
        
    
    let intersection = match answers.next() {
        Some(intersection) => intersection,
        _ => return 0
    };

    answers
        .fold(intersection, |acc, set| acc.intersection(&set).cloned().collect())
        .len()
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 06B");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let result: usize = contents.split("\n\n")
        .map(|group| count_answers(group))
        .sum();

    println!("{}", result);
}
