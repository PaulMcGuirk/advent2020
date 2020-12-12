-- Advent of Code 2020
-- Day 1 Problem 1

module Day01a where

parseInput :: [Char] -> [Int]
parseInput input = [read line | line <- lines input, not (null line)]

findPair :: [Int] -> Int -> Maybe (Int, Int)
findPair nums target = let solns = [(a, b) | a <- nums, b <- nums, a + b == target]
    in case solns of
        [] -> Nothing
        (x:_) -> Just x 


main :: IO()
main = do
    input <- readFile "input01.txt"
    putStrLn "Advent of Code 2020 1.1"

    let nums = parseInput input

    case findPair nums 2020 of
            Nothing -> putStrLn "No solution found"
            Just (a, b) -> print (a * b)