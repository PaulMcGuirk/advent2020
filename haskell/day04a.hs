-- Advent of Code 2020
-- Day 4 Problem 1

module Day04a where

import qualified Data.Map.Strict as Map

type Passport = Map.Map String String

parseInput :: String -> [Passport]
parseInput s = parseInputHelper (lines s) [] []
    where
        parseInputHelper :: [String] -> [(String, String)] -> [Passport] -> [Passport]
        parseInputHelper [] _ passports = passports
        parseInputHelper (l:ls) fields passports
            | null l =  parseInputHelper ls [] (passports ++ [Map.fromList fields])
            | otherwise = parseInputHelper ls (fields ++ fieldLineParse l) passports
        fieldLineParse :: String -> [(String, String)]
        fieldLineParse line = [fieldParse word | word <- words line]
        fieldParse :: String -> (String, String)
        fieldParse s = (head seg, (head . tail) seg)
            where seg = splitString s ':'

hasRequiredFields :: Passport -> [String] -> Bool
hasRequiredFields passport = all (`Map.member` passport)


main :: IO()
main = do
    input <- readFile "input04.txt"
    putStrLn "Advent of Code 2020 4.1"

    let passports = parseInput input
    let requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    let result = length [passport | passport <- passports, hasRequiredFields passport requiredFields]

    print result

splitString :: String -> Char -> [String]
splitString s splitChar = splitStringHelper s "" []
    where
        splitStringHelper :: [Char] -> [Char] -> [[Char]] -> [[Char]]
        splitStringHelper [] word words = words ++ [word]
        splitStringHelper (ch:chs) word words
            | ch == splitChar = splitStringHelper chs "" (words ++ [word])
            | otherwise = splitStringHelper chs (word ++ [ch]) words