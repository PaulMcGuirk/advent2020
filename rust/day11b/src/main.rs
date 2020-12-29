use std::fs;
use std::str::FromStr;
use std::fmt;

const FILEPATH: &str = "./input/input11.txt";

#[derive(Eq, PartialEq)]
enum TileState {
    Floor,
    Empty,
    Occupied
}

type BoardState = Vec<Vec<TileState>>;

struct Life {
    board: BoardState,
    num_rows: usize,
    num_cols: usize
}

impl Life {
    fn tick(&mut self) {
        
        self.board = (0..self.num_rows).map(|row| {
            (0..self.num_cols).map(|col| {
                let neighbor_count = self.count_neighbors(row, col);
                match self.board[row][col] {
                    TileState::Floor => TileState::Floor,
                    TileState::Empty => if neighbor_count == 0 { TileState::Occupied } else { TileState::Empty },
                    TileState::Occupied => if neighbor_count >= 5 { TileState::Empty } else { TileState::Occupied }
                }
            }).collect()
        }).collect();
    }

    fn count_neighbors(&self, row: usize, col: usize) -> usize {
        let mut result = 0;
        
        let mut delta_row = -1;
        while delta_row <= 1 {
            let mut delta_col = -1;
            while delta_col <= 1 {
                if delta_row == 0 && delta_col == 0 {
                    delta_col += 1;
                    continue;
                }
                for distance in 1.. {
                    let row = row as i32 + distance * delta_row;
                    let col = col as i32 + distance * delta_col;
                    if row < 0  || row >= self.num_rows as i32 || col < 0  || col >= self.num_cols as i32 {
                        break;
                    }
                    match self.board[row as usize][col as usize] {
                        TileState::Floor => continue,
                        TileState::Empty => break,
                        TileState::Occupied => {
                            result += 1;
                            break
                        }
                    }
                }
                delta_col += 1;
            }
            delta_row += 1;
        }

        result
    }

    fn count_occupied(&self) -> usize {
        self.board.iter().map(|row| row.iter().filter(|&cell| *cell == TileState::Occupied).count()).sum()
    }
}

impl fmt::Display for Life {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write! (f, "{}", self.board.iter().map(|row| row.iter().map(|cell| match cell {
            TileState::Floor => '.',
            TileState::Empty => 'L',
            TileState::Occupied => '#'
        }).collect::<String>()).collect::<Vec<String>>().join("\n"))
    }
}

impl FromStr for Life {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let state: Result<BoardState, _> = s.trim().lines()
            .map(|line| {
                line.chars().map(|ch| {
                    match ch {
                        '#' => Ok(TileState::Occupied),
                        '.' => Ok(TileState::Floor),
                        'L' => Ok(TileState::Empty),
                        _ => Err(format!("Unrecognized char {}", ch))
                    }
                }).collect()
            }).collect();
        
        let state = match state {
            Ok(state) => state,
            Err(s) => return Err(s)
        };

        Ok(Life {
            num_rows: state.len(),
            num_cols: state[0].len(),
            board: state,

        })
    }
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 11B");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let life = Life::from_str(&contents);

    let mut life = match life {
        Ok(life) => life,
        _ => {
            println!("Could not parse input");
            return
        }
    };

    let mut state = life.to_string();

    loop {
        life.tick();
        let new_state = life.to_string();
        if new_state == state {
            break;
        }
        state = new_state;
    }

    let result = life.count_occupied();

    println!("{}", result);
}