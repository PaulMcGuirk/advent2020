use std::fs;
use std::str::FromStr;

const FILEPATH: &str = "./input/input12.txt";

struct Vector {
    x: i32,
    y: i32
}

struct Ship {
    pos: Vector,
    way: Vector
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
enum Action {
    North,
    South,
    East,
    West,
    Left,
    Right,
    Forward
}

struct Instruction {
    action: Action,
    value: i32
}

impl FromStr for Instruction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let chars: Vec<char> = s.chars().collect();

        let action = match chars[0] {
            'N' => Action::North,
            'S' => Action::South,
            'E' => Action::East,
            'W' => Action::West,
            'L' => Action::Left,
            'R' => Action::Right,
            'F' => Action::Forward,
            _ => return Err(format!("Unrecognized action: {}", chars[0]))
        };

        let value = match chars[1..].iter().collect::<String>().parse::<i32>() {
            Ok(value) => value,
            _ => return Err(format!("Could not parse value for {}", s))
        };

        Ok(Instruction {
            action: action,
            value: value
        })
    }
}

impl Ship {
    fn navigate(&mut self, instructions: &Vec<Instruction>) {
        for instruction in instructions {

            match instruction.action {
                Action::North => self.way.y -= instruction.value,
                Action::South => self.way.y += instruction.value,
                Action::East => self.way.x += instruction.value,
                Action::West => self.way.x -= instruction.value,
                Action::Left => for _ in 0..(instruction.value / 90) { self.way = Vector { x: self.way.y, y: -self.way.x } },
                Action::Right => for _ in 0..(instruction.value / 90) { self.way = Vector { x: -self.way.y, y: self.way.x } },
                Action::Forward => self.pos = Vector {x: self.pos.x + instruction.value * self.way.x, y: self.pos.y + instruction.value * self.way.y }
            }
        }
    }

    fn get_distance(&self) -> i32 {
        self.pos.x.abs() + self.pos.y.abs()
    }
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 12B");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let instructions: Result<Vec<_>,_> = contents.trim().split('\n')
        .map(|line| Instruction::from_str(line))
        .collect();

    let instructions = match instructions {
        Ok(instructions) => instructions,
        Err(s) => {
            println!("{}", s);
            return;
        }
    };

    let mut ship = Ship {
        pos: Vector { x: 0, y: 0 },
        way: Vector { x: 10, y: -1 }
    };

    ship.navigate(&instructions);

    let result = ship.get_distance();

    println!("{}", result);
}