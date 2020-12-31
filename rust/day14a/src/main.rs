use std::fs;
use std::collections::HashMap;
use std::convert::TryInto;
use lazy_static::lazy_static;
use regex::Regex;

const FILEPATH: &str = "./input/input14.txt";
const ADDRESS_SIZE: usize = 36;

macro_rules! to_bitstring {
    ($num:expr) => { format!("{:036b}", $num)}
}

struct BitmaskComputer {
    bitmask: [char; ADDRESS_SIZE],
    memory: HashMap<i64, i64>
}

impl BitmaskComputer {
    fn new() -> BitmaskComputer {
        BitmaskComputer {
            bitmask: ['X'; ADDRESS_SIZE],
            memory: HashMap::new()
        }
    }

    fn run_instruction(&mut self, inst: &str) {
        lazy_static! {
            static ref MASK_REGEX: Regex = Regex::new(r"^mask = (?P<mask>[01X]{36})$").unwrap();
            static ref SET_REGEX: Regex = Regex::new(r"^mem\[(?P<address>\d+)\] = (?P<value>\d+)$").unwrap();
        }

        let caps = MASK_REGEX.captures(inst);
        if let Some(caps) = caps {
            self.bitmask = caps["mask"].chars().collect::<Vec<_>>().try_into().unwrap();
            return;
        }

        let caps = SET_REGEX.captures(inst);
        let caps = match caps {
            Some(caps) => caps,
            _ => panic!("Could not parse {}", inst)
        };

        let &address = &caps["address"].parse::<i64>().unwrap();
        let &value = &caps["value"].parse::<i64>().unwrap();

        let value = self.mask_value(value);

        self.memory.insert(address, value);
    }

    fn total_in_memory(&self) -> i64 {
        self.memory.values().sum()
    }

    fn mask_value(&self, value: i64) -> i64 {
        let bits = to_bitstring!(value).chars()
            .enumerate()
            .map(|(pos, ch)| if self.bitmask[pos] == 'X' { ch } else { self.bitmask[pos] })
            .collect::<String>();

        i64::from_str_radix(&bits, 2).unwrap()
    }
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 14A");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");
    
    let instructions = contents.trim().split('\n').collect::<Vec<_>>();

    let mut computer = BitmaskComputer::new();

    for instruction in instructions {
        computer.run_instruction(&instruction);
    }

    let result = computer.total_in_memory();

    println!("{}", result);
}