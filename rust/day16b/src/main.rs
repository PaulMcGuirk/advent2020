use std::fs;
use std::collections::{HashMap, HashSet};
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

fn identify_fields(
    tickets: &Vec<Vec<u32>>,
    field_ranges: &HashMap<String, Vec<Interval>>) -> HashMap<String, usize> {

    let tickets = tickets.iter().filter(|ticket| ticket.iter().all(|val| !is_invalid(*val, &field_ranges))).collect::<Vec<_>>();
    
    let num_fields = field_ranges.len();
    let mut all_possible_fields: Vec<HashSet<&String>> = vec!();
    
    for i in 0..num_fields {
        let all_fields = field_ranges.keys().collect::<HashSet<_>>();
        let possible_fields = tickets.iter().fold(all_fields, |possible_fields, ticket| {
            possible_fields.into_iter().filter(|&field| field_ranges[field].iter().any(|interval| interval.contains(ticket[i]))).collect()
        });
        all_possible_fields.push(possible_fields);
    }

    loop {
        let mut done = true;
        let mut removed = false;

        for i in 0..num_fields {
            let possible_fields = &all_possible_fields[i];
            if possible_fields.len() > 1 {
                done = false;
                continue;
            }
            if possible_fields.len() == 0 {
                panic!("Found an empty set of possibile fields");
            }

            let &field = possible_fields.iter().next().unwrap();

            for j in 0..num_fields {
                if i == j {
                    continue;
                }

                if all_possible_fields[j].contains(field) {
                    removed = true;
                    all_possible_fields[j].remove(field);
                }
            }
        }

        if done {
            break;
        }
        if !removed {
            panic!("Couldn't reduce the possibilities")
        }
    }

    all_possible_fields.into_iter().enumerate()
        .map(|(i, possible_fields)| ((*possible_fields.iter().next().unwrap()).clone(), i))
        .collect()
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 16B");

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

    let your_ticket = match segments.next() {
        Some(your_ticket) => your_ticket,
        _ => {
            println!("Your ticket expected");
            return
        }
    };

    let your_ticket = your_ticket.trim().split('\n').collect::<Vec<_>>();
    if your_ticket.len() != 2 {
        println!("Expected two lines for your ticket");
        return;
    }

    let your_ticket = match parse_ticket(&your_ticket[1]) {
        Ok(your_ticket) => your_ticket,
        _ => {
            println!("Could not parse your ticket");
            return
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

    let field_names = identify_fields(&nearby_tickets, &field_ranges);

    let result = field_names.iter().filter(|(field_name, _)| field_name.contains("departure"))
        .fold(1u64, |product, (_, &pos)| product * (your_ticket[pos] as u64));

    println!("{}", result);
}