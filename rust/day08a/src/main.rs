use std::fs;
use std::str::FromStr;
use std::collections::HashSet;

const FILEPATH: &str = "./input/input08.txt";

#[derive(Debug)]
enum Operation {
    Accumulate,
    NoOperation,
    Jump
}

#[derive(Debug)]
struct Instruction {
    op: Operation,
    arg: i32
}

impl FromStr for Instruction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut pieces = s.split(' ');

        let op = match pieces.next() {
            Some(op) => op,
            _ => return Err(format!("No operation given"))
        };

        let op = match op {
            "nop" => Operation::NoOperation,
            "acc" => Operation::Accumulate,
            "jmp" => Operation::Jump,
            _ => return Err(format!("Unrecognized operation '{}'", op))
        };

        let arg = match pieces.next() {
            Some(arg) => arg,
            _ => return Err(format!("No argument given for '{}'", s))
        };

        let arg = match arg.parse::<i32>() {
            Ok(arg) => arg,
            _ => return Err(format!("Could not parse argument: '{}'", arg))
        };

        if let Some(_) = pieces.next() {
            return Err(format!("Too many pieces in '{}'", s));
        }

        Ok(Instruction {
            op: op,
            arg: arg
        })
    }
}

fn run_instructions(instructions: &Vec<Instruction>, accumulator: i32) -> i32{
    let mut accumulator = accumulator;
    let mut pos = 0i32;

    let mut visited: HashSet<i32> = HashSet::new();

    loop {
        if visited.contains(&pos) {
            return accumulator;
        }
        visited.insert(pos);
        let inst = &instructions[pos as usize];

        match inst.op {
            Operation::Accumulate => { accumulator += inst.arg; pos += 1; }
            Operation::NoOperation => { pos += 1; }
            Operation::Jump => { pos += inst.arg; }
        }
    }

}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 08A");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let instructions: Result<Vec<_>,_> = contents.lines().map(Instruction::from_str).collect();
    let instructions = match instructions {
        Ok(instructions) => instructions,
        Err(s) => {
            println!("Could not parse instructions: {}", s);
            return;
        }
    };

    let result = run_instructions(&instructions, 0);

    println!("{}", result);
}