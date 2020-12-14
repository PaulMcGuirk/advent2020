-- Advent of Code 2020
-- Day 5 Part 1

module Day05a where

parseBoardingPass :: String -> Int
parseBoardingPass s = parseBoardingPassHelper s 0
    where
        parseBoardingPassHelper "" acc = acc
        parseBoardingPassHelper (x:xs) acc
            | x == 'B' || x == 'R' = parseBoardingPassHelper xs (2 * acc + 1)
            | otherwise = parseBoardingPassHelper xs (2 * acc)

main :: IO()
main = do
    input <- readFile "input05.txt"
    putStrLn "Advent of Code 2020 5.1"

    let boardingPassesIds = [parseBoardingPass line | line <- lines input, not (null line)]
    let result = maximum boardingPassesIds

    print result
