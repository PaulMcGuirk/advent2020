use std::fs;

const FILEPATH: &str = "./input/input18.txt";

fn evaluate(expr: &str) -> i64 {
    let expr = expr.replace("(", "( ");
    let expr = expr.replace(")", " )");
    let pieces = expr.split(" ").collect::<Vec<_>>();

    fn evaluate_helper(pieces: &Vec<&str>, pos: usize) -> (usize, i64) {
        let (mut pos, mut val) = if pieces[pos] == "(" {
            evaluate_helper(&pieces, pos + 1)
        } else {
            (pos + 1, pieces[pos].parse::<i64>().unwrap())
        };

        while pos < pieces.len() {
            if pieces[pos] == ")" {
                break;
            }

            let op = pieces[pos];
            pos += 1;

            let (next_pos, next) = if pieces[pos] == "(" {
                evaluate_helper(&pieces, pos + 1)
            } else {
                (pos + 1, pieces[pos].parse::<i64>().unwrap())
            };

            pos = next_pos;
            val = match op {
                "+" => val + next,
                "*" => val * next,
                _ => panic!("Invalid op {}", op)
            }

        }

        (pos + 1, val)
    }

    let (_, result) = evaluate_helper(&pieces, 0);
    result
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 18A");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let result = contents.trim()
        .lines()
        .map(evaluate)
        .sum::<i64>();

    println!("{}", result);

}