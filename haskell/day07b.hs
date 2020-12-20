{-# LANGUAGE OverloadedStrings #-}

-- Advent of Code 2020
-- Day 7 Part 2

module Day07a where

-- import Data.Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Read (readMaybe)
import Data.List (isInfixOf)

parseRules :: String -> Map.Map String [(Int, String)]
parseRules s = parseRulesHelper (lines s) Map.empty
    where
         parseRulesHelper [] rules = rules
         parseRulesHelper (l:ls) rules = parseRulesHelper ls (addToRules rules l)

addToRules :: Map.Map String [(Int, String)] -> String -> Map.Map String [(Int, String)]
addToRules rules s =
    let pair = breakLine s
    in uncurry (addToRulesHelper rules) pair
        where
            addToRulesHelper :: Map.Map String [(Int, String)] -> String -> [(Int, String)] -> Map.Map String [(Int, String)]
            addToRulesHelper rules _ [] = rules
            addToRulesHelper rules parent (child:children) = addToRulesHelper (Map.insertWith (++) parent [child] rules) parent children

breakLine :: String -> (String, [(Int, String)])
breakLine s = breakLineHelper [] (words s)
    where
        breakLineHelper _ [] = error "Expected the word 'contains'"
        breakLineHelper parentWords (w:ws)
            | w == "contain" = (unwords parentWords, if head ws == "no" then [] else parseChildren (unwords ws))
            | w == "bags" = breakLineHelper parentWords ws
            | otherwise = breakLineHelper (parentWords ++ [w]) ws
        parseChildren children = map parseChild (splitString children ',')

parseChild :: String -> (Int, String)
parseChild s = (read $ head sWords, parseChildHelper [] (tail sWords))
    where
        sWords = words s
        parseChildHelper :: [String] -> [String] -> String
        parseChildHelper _ [] = error "Expected bag or bags or something"
        parseChildHelper childWords (w:ws)
            | ("bag" :: String) `isInfixOf` w = unwords childWords
            | otherwise = parseChildHelper (childWords ++ [w]) ws

countChildren :: Map.Map String [(Int, String)] -> String -> Int
countChildren rules = countChildrenHelper
    where
        countChildrenHelper bag
            | bag `Map.member` rules = sum [count * (1 + countChildrenHelper child)| (count, child) <- rules Map.! bag]
            | otherwise = 0

main :: IO()
main = do
    input <- readFile "input07.txt" 
    putStrLn "Advent of Code 2020 7.2"

    let rules = parseRules input
    let result = countChildren rules "shiny gold"
    print result

splitString :: String -> Char -> [String]
splitString s splitChar = splitStringHelper s "" []
    where
        splitStringHelper :: [Char] -> [Char] -> [[Char]] -> [[Char]]
        splitStringHelper [] word words = words ++ [word]
        splitStringHelper (ch:chs) word words
            | ch == splitChar = splitStringHelper chs "" (words ++ [word])
            | otherwise = splitStringHelper chs (word ++ [ch]) words