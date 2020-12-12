-- Advent of Code 2020
-- Day 1 Problem 2

module Day01a where

parseInput :: [Char] -> [Int]
parseInput input = [read line | line <- lines input, not (null line)]

findTriple :: [Int] -> Int -> Maybe (Int, Int, Int)
findTriple nums target = let solns = [(a, b, c) | a <- nums, b <- nums, c <- nums, a + b + c == target]
    in case solns of
        [] -> Nothing
        (x:_) -> Just x 

findMultiple :: [Int] -> Int -> Int -> Maybe [Int]
findMultiple nums target size = findMultipleHelper nums target size []
    where
        findMultipleHelper :: [Int] -> Int -> Int -> [Int] -> Maybe [Int]
        findMultipleHelper [] target size working
            | target == 0 && size == 0 = Just working
            | otherwise = Nothing
        findMultipleHelper (n:ns) target size working
            | target < 0 = Nothing
            | target == 0 && size == 0 = Just working
            | otherwise = case findMultipleHelper ns (target - n) (size - 1) (working ++ [n]) of
                Just lst -> Just lst
                Nothing -> findMultipleHelper ns target size working

main :: IO()
main = do
    input <- readFile "input01.txt"
    putStrLn "Advent of Code 2020 1.2"

    let nums = parseInput input

    case findMultiple nums 2020 3 of
            Nothing -> putStrLn "No solution found"
            Just result -> print (foldl (*) 1 result)