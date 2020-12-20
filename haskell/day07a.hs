{-# LANGUAGE OverloadedStrings #-}

-- Advent of Code 2020
-- Day 7 Part 1

module Day07a where

-- import Data.Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Read (readMaybe)
import Data.Maybe
import Data.List (isInfixOf)

parseRules :: String -> Map.Map String [String]
parseRules s = parseRulesHelper (lines s) Map.empty
    where
        parseRulesHelper [] rules = rules
        parseRulesHelper (l:ls) rules = parseRulesHelper ls (addToRules rules l)

addToRules :: Map.Map String [String] -> String -> Map.Map String [String]
addToRules rules s =
    let pair = breakLine s
    in uncurry (addToRulesHelper rules) pair
        where
            addToRulesHelper :: Map.Map String [String] -> String -> [String] -> Map.Map String [String]
            addToRulesHelper rules _ [] = rules
            addToRulesHelper rules parent (child:children) = addToRulesHelper (Map.insertWith (++) child [parent] rules) parent children

breakLine :: String -> (String, [String])
breakLine s = breakLineHelper [] (words s)
    where
        breakLineHelper _ [] = error "Expected the word 'contains'"
        breakLineHelper parentWords (w:ws)
            | w == "contain" = (unwords parentWords, parseChildren (unwords ws))
            | w == "bags" = breakLineHelper parentWords ws
            | otherwise = breakLineHelper (parentWords ++ [w]) ws
        parseChildren children = map parseChild (splitString children ',')

parseChild :: String -> String
parseChild s = parseChildHelper [] (words s)
    where
        parseChildHelper :: [String] -> [String] -> String
        parseChildHelper _ [] = error "Expected bag or bags or something"
        parseChildHelper childWords (w:ws)
            | ("bag" :: String) `isInfixOf` w = unwords childWords
            | isNothing (readMaybe w :: Maybe Int) = parseChildHelper (childWords ++ [w]) ws
            | otherwise = parseChildHelper childWords ws

countAncestors :: Map.Map String [String] -> String -> Int
countAncestors rules bag = countAncestorsHelper (rules Map.! bag) Set.empty
    where
        countAncestorsHelper [] parents = length parents
        countAncestorsHelper (x:xs) parents
            | x `Set.member` parents = countAncestorsHelper xs parents
            | x `Map.notMember` rules = countAncestorsHelper xs (Set.insert x parents)
            | otherwise = countAncestorsHelper ((rules Map.! x) ++ xs) (Set.insert x parents)

main :: IO()
main = do
    input <- readFile "input07.txt" 
    putStrLn "Advent of Code 2020 7.1"

    let rules = parseRules input

    let result = countAncestors rules "shiny gold"

    print result

splitString :: String -> Char -> [String]
splitString s splitChar = splitStringHelper s "" []
    where
        splitStringHelper :: [Char] -> [Char] -> [[Char]] -> [[Char]]
        splitStringHelper [] word words = words ++ [word]
        splitStringHelper (ch:chs) word words
            | ch == splitChar = splitStringHelper chs "" (words ++ [word])
            | otherwise = splitStringHelper chs (word ++ [ch]) words