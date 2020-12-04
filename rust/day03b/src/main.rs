use std::fs;

const FILEPATH: &str = "./input/input03.txt";

struct Slope {
    delta_x: usize,
    delta_y: usize
}

enum TileType {
    Tree,
    Free
}

struct Forest {
    grid: Vec<Vec<TileType>>,
    num_rows: usize,
    num_cols: usize
}

impl Forest {
    fn from_string(s: &str) -> Forest {
        let grid: Vec<Vec<TileType>> = s.trim()
            .lines()
            .map(|line| Forest::row_from_string(line))
            .collect();

        let num_rows = grid.len();
        let num_cols = grid[0].len();

        Forest { grid, num_rows, num_cols }
    }

    fn row_from_string(s: &str) -> Vec<TileType> {
        s.chars()
            .map(|ch| match ch {
                '.' => TileType::Free,
                '#' => TileType::Tree,
                _ => panic!("Unrecognized char")
            })
            .collect()
    }

    fn count_trees(&self, slope: &Slope) -> usize {
        let mut result = 0;
        let mut x = 0;
        let mut y = 0;

        while y < self.num_rows {
            result = match self.grid[y][x] {
                TileType::Tree => result + 1,
                _ => result
            };
            x = (x + slope.delta_x) % self.num_cols;
            y = y + slope.delta_y;
        }
        result
    }
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 03B");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let forest = Forest::from_string(&contents);

    let slopes: Vec<Slope> = vec![
        Slope { delta_x: 1, delta_y: 1 },
        Slope { delta_x: 3, delta_y: 1 },
        Slope { delta_x: 5, delta_y: 1 },
        Slope { delta_x: 7, delta_y: 1 },
        Slope { delta_x: 1, delta_y: 2 }
    ];

    let result = slopes.iter()
        .map(|slope| forest.count_trees(&slope))
        .fold(1, |prod, val| prod * val);

    println!("{}", result);
}
