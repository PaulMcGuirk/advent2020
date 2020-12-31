use std::fs;
use std::collections::HashMap;
use lazy_static::lazy_static;
use regex::Regex;

const FILEPATH: &str = "./input/input16.txt";

#[derive(Debug)]
struct Interval {
    lower: u32,
    upper: u32
}

impl Interval {
    fn contains(&self, val: u32) -> bool {
        self.lower <= val && val <= self.upper
    }
}

fn parse_field_ranges(s: &str) -> Result<HashMap<String, Vec<Interval>>, String> {
    s.lines().map(parse_field_line).collect()
}

fn parse_field_line(s: &str) -> Result<(String, Vec<Interval>), String> {
    lazy_static! {
        static ref FIELD_RE: Regex = Regex::new(r"^(?P<field_name>[a-z ]+): (?P<min_one>\d+)-(?P<max_one>\d+) or (?P<min_two>\d+)-(?P<max_two>\d+)$").unwrap();
    }

    let caps = FIELD_RE.captures(s);
    let caps = match caps {
        Some(caps) => caps,
        _ => return Err(format!("Could not parse {}", s))
    };

    let field_name = &caps["field_name"];
    let min_one = caps["min_one"].parse::<u32>().unwrap();
    let max_one = caps["max_one"].parse::<u32>().unwrap();
    let min_two = caps["min_two"].parse::<u32>().unwrap();
    let max_two = caps["max_two"].parse::<u32>().unwrap();

    let interval_one = Interval { lower: min_one, upper: max_one };
    let interval_two = Interval { lower: min_two, upper: max_two };

    Ok((String::from(field_name), vec![interval_one, interval_two]))
}

fn parse_ticket(s: &str) -> Result<Vec<u32>, std::num::ParseIntError> {
    s.split(',').map(|seg| seg.parse::<u32>()).collect()
}

fn is_invalid(val: u32, field_ranges: &HashMap<String, Vec<Interval>>) -> bool {
    !field_ranges.values().any(|field_ranges| field_ranges.iter().any(|interval| interval.contains(val)))
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 16A");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let mut segments = contents.trim().split("\n\n");

    let field_ranges = match segments.next() {
        Some(field_ranges) => field_ranges,
        _ => {
            println!("No field data");
            return;
        }
    };

    let field_ranges = match parse_field_ranges(field_ranges) {
        Ok(field_ranges) => field_ranges,
        Err(s) => {
            println!("Could not parse field definitions: {}", s);
            return
        }
    };

    match segments.next() {
        Some(_) => (),
        _ => {
            println!("Your ticket expected");
        }
    };

    let nearby_tickets = match segments.next() {
        Some(nearby_tickets) => nearby_tickets,
        _ => {
            println!("No nearby tickets");
            return
        }
    };

    let nearby_tickets = nearby_tickets.trim().split('\n')
        .map(|line| parse_ticket(line))
        .filter_map(Result::ok)
        .collect::<Vec<Vec<u32>>>();

    let result = nearby_tickets.iter()
        .map(|ticket| ticket.iter().filter(|val| is_invalid(**val, &field_ranges)).sum::<u32>())
        .sum::<u32>();

    println!("{}", result);
}