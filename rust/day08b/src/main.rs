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

fn run_instructions(instructions: &Vec<&Instruction>, accumulator: i32) -> Option<i32> {
    let mut accumulator = accumulator;
    let mut pos = 0usize;

    let mut visited: HashSet<usize> = HashSet::new();

    loop {
        if pos >= instructions.len() {
            break;
        }
        if visited.contains(&pos) {
            return None;
        }
        visited.insert(pos);
        let inst = &instructions[pos as usize];

        match inst.op {
            Operation::Accumulate => { accumulator += inst.arg; pos += 1; }
            Operation::NoOperation => { pos += 1; }
            Operation::Jump => { pos = ((pos as i32) + inst.arg) as usize; }
        }
    }

    Some(accumulator)
}

fn repair_instructions(instructions: &Vec<Instruction>, accumulator: i32) -> Option<i32> {
    for i in 0..instructions.len() {
        let instruction = &instructions[i];
        let new_instruction = match instruction.op {
            Operation::Accumulate => continue,
            Operation::NoOperation => Instruction { op: Operation::Jump, arg: instruction.arg },
            Operation::Jump => Instruction { op: Operation::NoOperation, arg: instruction.arg }
        };
        let mut new_instructions: Vec<&Instruction> = instructions.iter().map(|inst| inst).collect();
        new_instructions[i] = &new_instruction;
        // new_instructions[i] = new_instruction;

        if let Some(result) = run_instructions(&new_instructions, accumulator) {
            return Some(result);
        }
    }

    None
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 08B");

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

    let result = match repair_instructions(&instructions, 0) {
        Some(result) => result,
        _ => {
            println!("No result found");
            return;
        }
    };

    println!("{}", result);
}