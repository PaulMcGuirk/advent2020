use std::fs;
use std::str::FromStr;
use std::collections::HashSet;

const FILEPATH: &str = "./input/input05.txt";

struct BoardingPass {
    _row: u32,
    _col: u32,
    _code: String,
    id: u32
}

impl FromStr for BoardingPass {
    type Err = String;

    fn from_str(code: &str) -> Result<Self, Self::Err> {
        if code.len() != 10 {
            return Err(format!("{} does not have 10 characters", code));
        }
        let row_code = &code[..7];
        let col_code = &code[7..];

        let row = match Self::decode_binary(row_code, 'F', 'B') {
            Ok(row) => row,
            Err(s) => return Err(s)
        };
        let col = match Self::decode_binary(col_code, 'L', 'R') {
            Ok(row) => row,
            Err(s) => return Err(s)
        };

        let boarding_pass = BoardingPass {
            _row: row,
            _col: col,
            _code: String::from(code),
            id: 8 * row + col
        };

        Ok(boarding_pass)
    }
}

impl BoardingPass {
    fn decode_binary(code: &str, zero: char, one: char) -> Result<u32, String> {
        let bits = code.replace(zero, "0").replace(one, "1");
        match u32::from_str_radix(&bits, 2) {
            Ok(n) => Ok(n),
            _ => Err(format!("Could not parse {}", code))
        }
    }
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 05B");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let boarding_passes: Result<Vec<_>, _> = contents.trim()
        .lines()
        .map(|line| BoardingPass::from_str(line))
        .collect();
    let boarding_passes = match boarding_passes {
        Ok(boarding_passes) => boarding_passes,
        Err(s) => {
            println!("{}", s);
            return;
        }
    };

    let boarding_pass_ids: HashSet<u32> = boarding_passes.iter()
        .map(|boarding_pass| boarding_pass.id)
        .collect();

    let result = match (8..(1024-8))
        .map(|id| id as u32)
        .filter(|&id| !boarding_pass_ids.contains(&id)
            && boarding_pass_ids.contains(&(id - 1))
            && boarding_pass_ids.contains(&(id + 1)))
        .next() {
            Some(result) => result,
            _ => {
                println!("No solution found");
                return
            }
        };
    
    println!("{}", result);
}
