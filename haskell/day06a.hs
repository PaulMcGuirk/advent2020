-- Advent of Code 2020
-- Day 6 Part 1

module Day06a where

import qualified Data.Set as Set

countYeses :: String -> Int
countYeses s = countYesesHelper (lines s) Set.empty 0
    where
        countYesesHelper [] working count = count + length working
        countYesesHelper (l:ls) working count
            | null l = countYesesHelper ls Set.empty count + length working
            | otherwise = countYesesHelper ls (working `Set.union` Set.fromList l) count

main :: IO()
main = do
    input <- readFile "input06.txt"
    putStrLn "Advent of Code 2020 6.1"

    let result = countYeses input

    print result
