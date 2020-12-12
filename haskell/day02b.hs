-- Advent of Code 2020
-- Day 2 Problem 1

module Day02b where

import Debug.Trace

data PasswordEntry = PasswordEntry {
    a :: Int,
    b :: Int,
    ch :: Char,
    password :: String } deriving (Show)

parseEntry :: String -> PasswordEntry
parseEntry s = PasswordEntry {
    a = (read . head) rangePieces,
    b = (read . head . tail) rangePieces,
    ch = head (pieces !! 1),
    password = pieces !! 2}
    where
        pieces = words s
        range = head pieces
        rangePieces = splitString range '-'

isValid :: PasswordEntry -> Bool
isValid PasswordEntry {a = a, b = b, ch = ch, password = password } = (password !! (a - 1) == ch) /= (password !! (b - 1) == ch)

countIf :: (a -> Bool) -> [a] -> Int
countIf p elems = countIfHelper elems 0
    where
        countIfHelper [] count = count
        countIfHelper (x:xs) count
            | p x = countIfHelper xs (count + 1)
            | otherwise = countIfHelper xs count

countChar :: String -> Char -> Int
countChar word ch = countIf (== ch) word

main :: IO()
main = do
    input <- readFile "input02.txt"
    putStrLn "Advent of Code 2020 2.2"

    let entries = [parseEntry line | line <- lines input, not (null line)]
    let result = countIf isValid entries
    print result

splitString :: [Char] -> Char -> [[Char]]
splitString s splitChar = splitStringHelper s "" []
    where
        splitStringHelper :: [Char] -> [Char] -> [[Char]] -> [[Char]]
        splitStringHelper [] word words = words ++ [word]
        splitStringHelper (ch:chs) word words
            | ch == splitChar = splitStringHelper chs "" (words ++ [word])
            | otherwise = splitStringHelper chs (word ++ [ch]) words