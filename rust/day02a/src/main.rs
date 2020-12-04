use std::fs;
use regex::Regex;
use lazy_static::lazy_static;

const FILEPATH: &str = "./input/input02.txt";

#[derive(Debug)]
struct PasswordLine {
    min: usize,
    max: usize,
    req_char: char,
    password: String
}

impl PasswordLine {
    fn from_string(s: &str) -> Result<PasswordLine, String> {
        lazy_static! {
            static ref PASSWORD_RE: Regex = Regex::new(r"^(?P<min>\d+)-(?P<max>\d+) (?P<req_char>[a-z]): (?P<password>[a-z]+)$").unwrap();
        }

        let caps = match PASSWORD_RE.captures(s) {
            Some(caps) => caps,
            _ => return Err(format!("Line did mot match regex: {}", s))
        };

        let min = caps["min"].parse::<usize>().unwrap();
        let max = caps["max"].parse::<usize>().unwrap();
        let req_char = caps["req_char"].chars().next().unwrap();
        let password = String::from(&caps["password"]);

        let password_line = PasswordLine { min, max, req_char, password };
        Ok(password_line)
    }

    fn is_valid(&self) -> bool {
        let count = self.password.chars().filter(|&ch| ch == self.req_char).count();
        self.min <= count && count <= self.max
    }
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 02A");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let password_lines: Result<Vec<_>, _> = contents.trim().lines()
            .map(|line| PasswordLine::from_string(line))
            .collect();

    let password_lines = match password_lines {
        Ok(password_lines) => password_lines,
        _ => {
            println!("Couldn't parse");
            return;
        }
    };

    let result = password_lines.iter().filter(|password_line| password_line.is_valid()).count();
    println!("{}", result);
}
