use std::fs;
use std::collections::HashMap;

const FILEPATH: &str = "./input/input04.txt";

struct Passport {
    fields: HashMap<String, String>,
}

impl Passport {

    const REQUIRED_FIELDS: &'static [&'static str] = &["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

    fn parse_cache(cache_str: &str) -> Result<Vec<Passport>, String> {
        cache_str.split("\n\n")
            .map(|s| Self::from_string(&s))
            .collect()
    }

    fn from_string(s: &str) -> Result<Passport, String> {
        let fields: Result<HashMap<String, String>, String> = s.split_whitespace()
            .map(|s| Self::pair_from_string(s))
            .collect();
        
        let fields = match fields {
            Ok(fields) => fields,
            Err(s) => return Err(s)
        };

        Ok(Passport { fields })
    }

    fn pair_from_string(s: &str) -> Result<(String, String), String> {
        let mut segs = s.split(":");
        let a = match segs.next() {
            Some(a) => a,
            _ => return Err(format!("Not enough pieces in {}", s))
        };
        let b = match segs.next() {
            Some(b) => b,
            _ => return Err(format!("Not enough pieces in {}", s))
        };

        match segs.next() {
            Some(_) => return Err(format!("Too many pieces in {}", s)),
            _ => Ok((String::from(a), String::from(b)))
        }
    }

    fn is_valid(&self) -> bool {
        Self::REQUIRED_FIELDS.iter().all(|&f| self.fields.contains_key(f))
    }
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 04A");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let passports = match Passport::parse_cache(&contents) {
        Ok(passports) => passports,
        Err(s) => {
            println!("{}", s);
            return
        }
    };

    let result = passports.iter().filter(|passport| passport.is_valid()).count();

    println!("{}", result);
}
