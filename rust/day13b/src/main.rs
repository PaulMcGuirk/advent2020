use std::fs;

const FILEPATH: &str = "./input/input13.txt";

fn main() {
    println!("Advent of Code 2020");
    println!("Day 13B");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let lines: Vec<_> = contents.lines().collect();

    let primes: Vec<_> = lines[1].split(',')
        .enumerate()
        .filter(|pair| pair.1 != "x")
        .map(|pair| (pair.0, pair.1.parse::<i32>().unwrap()))
        .collect();

    let mut result = 0i64;
    let mut product = 1i64;

    for pair in primes {
        let (offset, prime) = pair;
        let prime = prime as i64;
        let offset = offset as i64;
        let mod_ = if offset == 0 { 0 } else { prime - (offset % prime) };
        while result % prime != mod_ {
            result += product;
        }
        product *= prime;
    }

    println!("{}", result);
}