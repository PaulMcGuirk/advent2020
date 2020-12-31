use std::fs;

const FILEPATH: &str = "./input/input15.txt";

fn number_game(nums: &Vec<usize>, last_turn: usize) -> usize {
    let mut mentions = vec![0usize; 10000];

    let mut turn = 1usize;
    let mut num: usize;

    while turn <= nums.len()
    {
        num = nums[turn - 1];
        mentions[num] = turn;
        turn += 1;
    }

    num = 0;
    let mut previous_mention = 0usize;

    while turn <= last_turn {

        num = if previous_mention == 0 { 0 }  else { turn - 1 - previous_mention };
        if num >= mentions.len() {
            mentions.resize(2 * mentions.len(), 0);
        }

        previous_mention = mentions[num];
        mentions[num] = turn;
        turn += 1;
    }

    num
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 15B");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");
    
    let nums = contents.trim()
        .split(',')
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<_>>();

    let result = number_game(&nums, 30000000);

    println!("{}", result);
}