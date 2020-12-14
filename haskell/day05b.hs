-- Advent of Code 2020
-- Day 5 Part 2

module Day05b where

import qualified Data.Set as Set

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
    putStrLn "Advent of Code 2020 5.2"

    let boardingPassesIds = Set.fromList [parseBoardingPass line | line <- lines input, not (null line)]
    let result = head [n | n <- [8..(1024 - 8)], n `Set.notMember` boardingPassesIds
                                    && (n - 1) `Set.member` boardingPassesIds
                                    && (n + 1) `Set.member` boardingPassesIds]

    print result
