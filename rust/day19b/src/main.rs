use std::fs;
use std::collections::HashMap;
use std::str::FromStr;

const FILEPATH: &str = "./input/input19.txt";

#[derive(Debug)]
enum Rule {
    Literal(char),
    Branches(Vec<Vec<usize>>)
}

impl FromStr for Rule {
    type Err = String;
    
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut chars = s.chars();
        if chars.next().unwrap() == '"' {
            return Ok(Rule::Literal(chars.next().unwrap()));
        }

        let branches = s.split('|')
            .map(|bs| {
                bs.trim()
                .split(' ')
                .map(|seg| seg.parse::<usize>().unwrap())
                .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        Ok(Rule::Branches(branches))
    }
}

#[derive(Debug)]
struct RuleSet {
    rules: HashMap<usize, Rule>
}

impl RuleSet {
    fn new(rule_strings: Vec<&str>) -> Self {
        let mut rules = HashMap::new();

        for s in rule_strings {
            let mut pieces = s.split(": ");
            let id = pieces.next().unwrap().parse::<usize>().unwrap();
            let rule = Rule::from_str(pieces.next().unwrap()).unwrap();
            rules.insert(id, rule);
        }

        RuleSet {
            rules: rules
        }
    }

    fn matches(&self, rule_id: usize, s: &str) -> bool {
        let chars = s.chars().collect::<Vec<_>>();

        let mut to_check = vec![(vec![rule_id], 0)];

        while let Some(pair) = to_check.pop() {
            let (mut rule_ids, pos) = pair;

            let rule_id = match rule_ids.pop() {
                Some(rule_id) => rule_id,
                _ => if pos == chars.len() { return true; } else { continue; }
            };

            if pos >= chars.len() {
                continue;
            }

            let rule = &self.rules[&rule_id];

            match rule {
                Rule::Literal(ch) => {
                    if chars[pos] == *ch {
                        to_check.push((rule_ids, pos + 1));
                    }
                },
                Rule::Branches(branches) => {
                    for branch in branches {
                        let mut to_add = rule_ids.to_vec();
                        to_add.extend(branch.iter().rev());
                        to_check.push((to_add, pos));
                    }
                }
            }
        }

        false
    }
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 19B");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let mut pieces = contents.split("\n\n");

    let mut rule_strings = pieces.next().unwrap().trim().lines().collect::<Vec<_>>();

    rule_strings.push("8: 42 | 42 8"); // these will overwrite the existing rules
    rule_strings.push("11: 42 31 | 42 11 31");
    
    let rules = RuleSet::new(rule_strings);

    let result = pieces.next().unwrap().trim().lines()
        .filter(|line| rules.matches(0, line))
        .count();
    
    println!("{}", result);

}