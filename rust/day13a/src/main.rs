use std::fs;

const FILEPATH: &str = "./input/input13.txt";

fn get_wait_time(earliest_departure_time: i32, bus_id: i32) -> i32 {
    let remainder = earliest_departure_time % bus_id;
    if remainder == 0 { 0 } else { bus_id - remainder }
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 13A");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let lines: Vec<_> = contents.lines().collect();

    let earliest_departure_time = lines[0].parse::<i32>().unwrap();
    let bus_ids: Vec<i32> = lines[1].split(',')
        .map(|s| s.parse::<i32>())
        .filter_map(Result::ok)
        .collect();

    let earliest = bus_ids.iter()
        .map(|&bus_id| (bus_id, get_wait_time(earliest_departure_time, bus_id)))
        .min_by_key(|pair| pair.1)
        .unwrap();


    let result = earliest.0 * earliest.1;

    println!("{}", result);
}