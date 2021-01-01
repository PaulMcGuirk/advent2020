use std::fs;
use std::collections::{HashSet};

const FILEPATH: &str = "./input/input17.txt";
const NUM_DIMS: usize = 4;

type Point = [i32; NUM_DIMS];

struct Life {
    active: HashSet<Point>
}

impl Life {

    fn parse(s: &str) -> Life {
        let active = s.trim().lines().enumerate()
            .flat_map(|(row, line)| {
                line.chars().enumerate().filter_map(move |(col, ch)| match ch {
                    '#' => {
                        let mut pt = [0i32; NUM_DIMS];
                        pt[0] = col as i32;
                        pt[1] = row as i32;
                        Some(pt)
                    }
                    _ => None
                })
            }).collect::<HashSet<_>>();

        Life {
            active: active
        }
    }

    fn tick(&mut self) {
        self.active = self.active.iter()
            .flat_map(Self::get_neighbors)
            .filter(|pt| self.next_state(&pt))
            .collect()
    }

    fn next_state(&self, pt: &Point) -> bool {
        let num_active_neighbors = Self::get_neighbors(pt)
            .intersection(&self.active).count(); // if pt is active, it's included here

        if self.active.contains(pt) {
            num_active_neighbors == 3 || num_active_neighbors == 4
        } else {
            num_active_neighbors == 3
        }
    }

    fn get_neighbors(pt: &Point) -> HashSet<Point> {
        let mut neighbors: HashSet<Point> = HashSet::new();
        neighbors.insert(pt.clone());

        for i in 0..NUM_DIMS {
            neighbors = neighbors.into_iter()
                .flat_map(|q| {
                    (0..3).map(move |j| {
                        let mut r = q;
                        r[i] = pt[i] + j - 1;
                        r
                    })
                })
                .collect();
        }
        neighbors
    }

    fn run(&mut self, num_runs: usize) {
        for _ in 0..num_runs {
            self.tick();
        }
    }
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 17B");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let mut life = Life::parse(&contents);

    life.run(6);

    let result = life.active.len();

    println!("{}", result);

}