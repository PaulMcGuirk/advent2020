-- Advent of Code 2020
-- Day 3 Problem 2

module Day03b where

import Data.Array

data Tile = Tree | Free deriving(Show, Eq)

type Forest = Array(Int, Int) Tile

parseInput :: String -> Forest
parseInput s = let forestList = [ [ if ch == '#' then Tree else Free | ch <- line ] | line <- lines s, not (null line)]
    in array ((0,0), ( (length . head) forestList - 1, length forestList - 1))
        ([((i, j), forestList !! j !! i) | i <- [0..(length . head) forestList - 1],
            j <- [0..(length forestList - 1)]])

countTrees :: Forest -> Int -> Int -> Int
countTrees forest deltaX deltaY = countTreesHelper 0 0 0
    where
        ((_, _), (maxRow, maxCol)) = bounds forest
        countTreesHelper :: Int -> Int -> Int -> Int
        countTreesHelper x y count
            | y > maxCol = count
            | forest!(x, y) == Tree = countTreesHelper ((x + deltaX) `mod` (1 + maxRow)) (y + deltaY) (count + 1)
            | otherwise = countTreesHelper ((x + deltaX) `mod` (1 + maxRow)) (y + deltaY) count

main :: IO()
main = do
    input <- readFile "input03.txt"
    putStrLn "Advent of Code 2020 3.2"

    let forest = parseInput input
    let counts = [countTrees forest deltaX deltaY | (deltaX, deltaY) <- [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] ]
    let result = foldl (*) 1 counts

    print result