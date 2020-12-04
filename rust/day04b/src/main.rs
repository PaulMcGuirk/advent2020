use std::fs;
use std::collections::HashMap;
use std::str::FromStr;
use lazy_static::lazy_static;
use regex::Regex;

const FILEPATH: &str = "./input/input04.txt";

#[derive(Hash, PartialEq, Eq, Debug)]
enum FieldTypes {
    BirthYear,
    IssuedYear,
    ExpiresYear,
    Height,
    HairColor,
    EyeColor,
    PassportId,
    Other(String)
}

impl FromStr for FieldTypes {

    type Err = ();

    fn from_str(s: &str) -> Result<FieldTypes, Self::Err> {
        use FieldTypes::*;
        match s {
            "byr" => Ok(BirthYear),
            "iyr" => Ok(IssuedYear),
            "eyr" => Ok(ExpiresYear),
            "hgt" => Ok(Height),
            "hcl" => Ok(HairColor),
            "ecl" => Ok(EyeColor),
            "pid" => Ok(PassportId),
            s => Ok(Other(String::from(s)))
        }
    }
}

impl FieldTypes {

    fn validate_value(&self, val: &str) -> bool {
        use FieldTypes::*;
        match self {
            BirthYear => Self::validate_year(val, 1920, 2002),
            IssuedYear => Self::validate_year(val, 2010, 2020),
            ExpiresYear => Self::validate_year(val, 2020, 2030),
            Height => Self::validate_height(val),
            HairColor => Self::validate_hair_color(val),
            EyeColor => Self::validate_eye_color(val),
            PassportId => Self::validate_passport_id(val),
            Other(_) => true
        }
    }

    fn validate_year(val: &str, min: i32, max: i32) -> bool {
        let val = match val.parse::<i32>() {
            Ok(val) => val,
            _ => return false
        };

        min <= val && val <= max
    }

    fn validate_height(val: &str) -> bool {
        lazy_static! {
            static ref HEIGHT_RE: Regex = Regex::new(r"^(?P<value>\d+)(?P<units>in|cm)$").unwrap();
        }
        let caps = match HEIGHT_RE.captures(val) {
            Some(caps) => caps,
            _ => return false
        };

        let value = caps["value"].parse::<i32>().unwrap();
        let units = &caps["units"];

        match units {
            "in" => 59 <= value && value <= 76,
            "cm" => 150 <= value && value <= 193,
            _ => false
        }
    }

    fn validate_hair_color(val: &str) -> bool {
        lazy_static! {
            static ref HAIR_COLOR_RE: Regex = Regex::new(r"^#[a-f0-9]{6}$").unwrap();
        }
        HAIR_COLOR_RE.is_match(val)
    }

    const VALID_EYE_COLORS: &'static[&'static str] = &["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];
    fn validate_eye_color(val: &str) -> bool {
        Self::VALID_EYE_COLORS.contains(&val)
    }

    fn validate_passport_id(val: &str) -> bool {
        lazy_static! {
            static ref PASSPORT_ID_RE: Regex = Regex::new(r"^[0-9]{9}$").unwrap();
        }
        PASSPORT_ID_RE.is_match(val)
    }
}

#[derive(Debug)]
struct Passport {
    fields: HashMap<FieldTypes, String>,
}

impl Passport {

    const REQUIRED_FIELDS: &'static [&'static FieldTypes] = &[
        &FieldTypes::BirthYear,
        &FieldTypes::IssuedYear,
        &FieldTypes::ExpiresYear,
        &FieldTypes::Height,
        &FieldTypes::HairColor,
        &FieldTypes::EyeColor,
        &FieldTypes::PassportId
    ];

    fn parse_cache(cache_str: &str) -> Result<Vec<Passport>, String> {
        cache_str.split("\n\n")
            .map(|s| Self::from_string(&s))
            .collect()
    }

    fn from_string(s: &str) -> Result<Passport, String> {
        let fields: Result<HashMap<FieldTypes, String>, String> = s.split_whitespace()
            .map(|s| Self::pair_from_string(s))
            .collect();
        
        let fields = match fields {
            Ok(fields) => fields,
            Err(s) => return Err(s)
        };

        let passport = Passport { fields };
        Ok(passport)
    }

    fn pair_from_string(s: &str) -> Result<(FieldTypes, String), String> {
        let mut segs = s.split(":");
        let a = match segs.next() {
            Some(a) => a,
            _ => return Err(format!("Not enough pieces in {}", s))
        };
        let b = match segs.next() {
            Some(b) => b,
            _ => return Err(format!("Not enough pieces in {}", s))
        };

        if segs.next() != None {
            return Err(format!("Too many pieces in {}", s));
        }

        let field = match FieldTypes::from_str(a) {
            Ok(field) => field,
            _ => return Err(format!("Could not parse {}", a))
        };
        
        Ok((field, String::from(b)))
    }

    fn is_valid(&self) -> bool {
        Self::REQUIRED_FIELDS.iter().all(|&f| self.fields.contains_key(f)
            && f.validate_value(&self.fields[f]))
    }
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 04B");

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
