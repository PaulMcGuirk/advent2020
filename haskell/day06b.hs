-- Advent of Code 2020
-- Day 6 Part 2

module Day06a where

import qualified Data.Set as Set

countYeses :: String -> Int
countYeses s = countYesesHelper (lines s) (Set.fromList ['a'..'z']) 0
    where
        countYesesHelper [] working count = count + length working
        countYesesHelper (l:ls) working count
            | null l = countYesesHelper ls (Set.fromList ['a'..'z']) count + length working
            | otherwise = countYesesHelper ls (working `Set.intersection` Set.fromList l) count

main :: IO()
main = do
    input <- readFile "input06.txt"
    putStrLn "Advent of Code 2020 6.2"

    let result = countYeses input

    print result
