use std::fs;
use std::str::FromStr;

const FILEPATH: &str = "./input/input12.txt";

#[derive(Copy, Clone, Debug)]
enum Heading {
    East = 0,
    South = 1,
    West = 2,
    North = 3
}

impl Heading {
    fn from_i32(val: i32) -> Option<Heading> {
        match val {
            0 => Some(Heading::East),
            1 => Some(Heading::South),
            2 => Some(Heading::West),
            3 => Some(Heading::North),
            _ => None
        }
    }
}

struct Ship {
    x: i32,
    y: i32,
    heading: Heading
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
            let action = if instruction.action == Action::Forward {
                Self::heading_to_action(self.heading)
            } else {
                instruction.action
            };

            match action {
                Action::North => self.y -= instruction.value,
                Action::South => self.y += instruction.value,
                Action::East => self.x += instruction.value,
                Action::West => self.x -= instruction.value,
                Action::Left => self.heading = Heading::from_i32((self.heading as i32 + 4 - instruction.value / 90) % 4).unwrap(),
                Action::Right => self.heading = Heading::from_i32((self.heading as i32 + 4 + instruction.value / 90) % 4).unwrap(),
                Action::Forward => panic!("Didn't handle forward correctly")
            }
        }
    }

    fn heading_to_action(heading: Heading) -> Action {
        match heading {
            Heading::North => Action::North,
            Heading::East => Action::East,
            Heading::West => Action::West,
            Heading::South => Action::South
        }
    }

    fn get_distance(&self) -> i32 {
        self.x.abs() + self.y.abs()
    }
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 12A");

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
        x: 0,
        y: 0,
        heading: Heading::East
    };

    ship.navigate(&instructions);

    let result = ship.get_distance();

    println!("{}", result);
}