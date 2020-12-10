use std::fs;
use std::collections::HashMap;
use std::str::FromStr;
use lazy_static::lazy_static;
use regex::Regex;

const FILEPATH: &str = "./input/input07.txt";

struct LuggageRules {
    _parents: HashMap<String, Vec<String>>,
    children: HashMap<String, Vec<(String, usize)>>
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
            children: all_children,
            _parents: all_parents
        })
    }
}

impl LuggageRules {
    fn count_children_with_quantity(&self, bag: &str) -> usize {
        let bag = String::from(bag);
        let mut counts: HashMap<&String, usize> = HashMap::new();
        let mut to_visit: Vec<_> = vec![&bag];

        while !counts.contains_key(&bag) && to_visit.len() > 0 {
            let inner_bag = to_visit.last().unwrap();
            if counts.contains_key(inner_bag)
            {
                to_visit.pop();
                continue;
            }

            let children: Vec<_> = self.children[*inner_bag].iter()
                .filter(|(child, _)| !counts.contains_key(child))
                .collect();
            
            if children.len() > 0 {
                for (child, _) in children {
                    to_visit.push(child);
                }
                continue
            }

            let inner_bag_count = self.children[*inner_bag].iter()
                .map(|(color, count)| count * (1 + counts[color]))
                .sum();
            counts.insert(inner_bag, inner_bag_count);

            to_visit.pop();
        }
        
        counts[&bag]
    }
}

fn main() {
    println!("Advent of Code 2020");
    println!("Day 07B");

    let contents = fs::read_to_string(FILEPATH)
        .expect("Error reading file.");

    let luggage_rules = match LuggageRules::from_str(&contents) {
        Ok(luggage_rules) => luggage_rules,
        Err(s) => {
            println!("{}", s);
            return;
        }
    };

    let result = luggage_rules.count_children_with_quantity("shiny gold");
    
    println!("{}", result);
}