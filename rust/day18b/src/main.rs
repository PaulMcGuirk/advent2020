use std::fs;

const FILEPATH: &str = "./input/input18.txt";

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum OperatorType {
    Addition,
    Multiplication

}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum ExpressionPart {
    SubexpressionStart,
    SubexpressionEnd,
    Number(i64),
    Operator(OperatorType)
}

type Expression = Vec<ExpressionPart>;

fn evaluate(expr: &str) -> i64 {
    let pieces = parse_tokens(expr);

    fn evaluate_expression(expr: &Expression) -> i64 {

        fn evaluate_subexpressions(expr: &Expression) -> Expression {
            let mut result = vec!();
            let mut pos = 0;

            while pos < expr.len() {
                if expr[pos] != ExpressionPart::SubexpressionStart {
                    result.push(expr[pos]);
                    pos += 1;
                    continue;
                }
                
                let mut right_pos = pos + 1;
                let mut stack_level = 1;

                loop {
                    stack_level = match expr[right_pos] {
                        ExpressionPart::SubexpressionStart => stack_level + 1,
                        ExpressionPart::SubexpressionEnd => stack_level - 1,
                        _ => stack_level
                    };
                    if stack_level == 0 {
                        break
                    }
                    right_pos += 1;
                }

                let sub_expr = expr[(pos + 1)..right_pos].to_vec();
                let val = evaluate_expression(&sub_expr);
                result.push(ExpressionPart::Number(val));
                pos = right_pos + 1;
            }

            result
        
        }

        fn evaluate_operator(expr: &Expression, op_to_eval: OperatorType) -> Expression {
            let mut result = vec![expr[0]];
            let mut pos = 1;

            while pos < expr.len() {
                let op = match expr[pos] {
                    ExpressionPart::Operator(op) => op,
                    _ => panic!("Parse error")
                };

                if op != op_to_eval {
                    result.push(expr[pos]);
                    result.push(expr[pos + 1]);
                    pos += 2;
                    continue;
                }

                let lhs = match result.pop().unwrap() {
                    ExpressionPart::Number(lhs) => lhs,
                    _ => panic!("parse error")
                };

                let rhs = match expr[pos + 1] {
                    ExpressionPart::Number(rhs) => rhs,
                    _ => panic!("parse error")
                };

                let val = match op_to_eval {
                    OperatorType::Addition => lhs + rhs,
                    OperatorType::Multiplication => lhs * rhs
                };

                result.push(ExpressionPart::Number(val));
                pos += 2;
            }

            result
        }

        let expr = evaluate_subexpressions(expr);
        let expr = evaluate_operator(&expr, OperatorType::Addition);
        let expr = evaluate_operator(&expr, OperatorType::Multiplication);

        match expr[0] {
            ExpressionPart::Number(n) => n,
            _ => panic!("expected number")
        }
    }

    evaluate_expression(&pieces)
}

fn parse_tokens(expr: &str) -> Expression {
    let expr = expr.replace("(", "( ");
    let expr = expr.replace(")", " )");
    expr.split(" ").map(|seg| match seg {
        "(" => ExpressionPart::SubexpressionStart,
        ")" => ExpressionPart::SubexpressionEnd,
        "+" => ExpressionPart::Operator(OperatorType::Addition),
        "*" => ExpressionPart::Operator(OperatorType::Multiplication),
        _ => ExpressionPart::Number(seg.parse::<i64>().unwrap())
    })
    .collect()
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 18B");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let result = contents.trim()
        .lines()
        .map(evaluate)
        .sum::<i64>();

    println!("{}", result);

}