use std::fs;
use std::collections::{HashMap, HashSet};
use std::str::FromStr;
use lazy_static::lazy_static;
use regex::Regex;

const FILEPATH: &str = "./input/input07.txt";

struct LuggageRules {
    parents: HashMap<String, Vec<String>>,
    _children: HashMap<String, Vec<(String, usize)>>
}

impl FromStr for LuggageRules {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref RULE_REGEX: Regex = Regex::new(r"^(?P<bag_color>[a-z ]+) bags contain (?P<children>[a-z0-9, ]+).$").unwrap();
            static ref CHILD_REGEX: Regex = Regex::new(r"(?P<amount>\d+) (?P<color>[a-z ]+) bag").unwrap();
        }

        let mut all_parents: HashMap<String, Vec<String>> = HashMap::new();
        let mut all_children: HashMap<String, Vec<(String, usize)>> = HashMap::new();

        for line in s.trim().lines() {
            let caps = match RULE_REGEX.captures(line) {
                Some(caps) => caps,
                _ => return Err(format!("Could not parse '{}'", line))
            };
            let color = &caps["bag_color"];

            let children: Vec<_> = CHILD_REGEX.captures_iter(&caps["children"])
                .map(|caps| (String::from(&caps["color"]), caps["amount"].parse::<usize>().unwrap()))
                .collect();
            
            for (child, _) in &children {
                if !all_parents.contains_key(child) {
                    all_parents.insert(child.clone(), vec!());
                }
                all_parents.get_mut(child).unwrap().push(String::from(color));
            }
            
            all_children.insert(String::from(color), children);
        }
        Ok(LuggageRules {
            _children: all_children,
            parents: all_parents
        })
    }
}

impl LuggageRules {
    fn count_ancestors(&self, bag: &str) -> usize {
        let mut ancestors_to_visit: Vec<_> = self.parents[bag].iter().collect();

        let mut ancestors: HashSet<&String> = HashSet::new();

        while ancestors_to_visit.len() > 0 {
            let ancestor = ancestors_to_visit.pop().unwrap();
            if ancestors.contains(ancestor) {
                continue;
            }
            ancestors.insert(ancestor);
            if !self.parents.contains_key(ancestor) {
                continue;
            }
            for parent in &self.parents[ancestor] {
                ancestors_to_visit.push(parent);
            }
        }

        ancestors.len()
    }
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 07A");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let luggage_rules = match LuggageRules::from_str(&contents) {
        Ok(luggage_rules) => luggage_rules,
        Err(s) => {
            println!("{}", s);
            return;
        }
    };

    let result = luggage_rules.count_ancestors("shiny gold");
    
    println!("{}", result);
}