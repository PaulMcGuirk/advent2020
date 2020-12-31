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

        for masked_address in self.get_masked_addresses(address).iter() {
            self.memory.insert(*masked_address, value);
        }
    }

    fn get_masked_addresses(&self, address: i64) -> Vec<i64> {
        let mut masked_bits: Vec<Vec<char>> = vec![vec![]];

        let address_bits: Vec<_> = to_bitstring!(address).chars().collect();

        for i in 0..ADDRESS_SIZE {
            masked_bits = masked_bits.iter().flat_map(|bits| {
                let bits_to_add = match self.bitmask[i] {
                    '0' => vec![address_bits[i]],
                    '1' => vec!['1'],
                    'X' => vec!['0', '1'],
                    _ => panic!("Unexpected bit")
                };
                bits_to_add.iter().map(|new_bit| {
                    let mut new_bits = bits.to_vec();
                    new_bits.push(*new_bit);
                    new_bits
                }).collect::<Vec<_>>()
            }).collect();
        }

        masked_bits.iter().map(|bits| i64::from_str_radix(&bits.iter().collect::<String>(), 2).unwrap()).collect()
    }

    fn total_in_memory(&self) -> i64 {
        self.memory.values().sum()
    }
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 14B");

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