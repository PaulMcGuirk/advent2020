-- Advent of Code 2020
-- Day 4 Problem 2

module Day04a where

import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)
import Data.Char (isDigit)

type Passport = Map.Map String String

parseInput :: String -> [Passport]
parseInput s = parseInputHelper (lines s) [] []
    where
        parseInputHelper :: [String] -> [(String, String)] -> [Passport] -> [Passport]
        parseInputHelper [] _ passports = passports
        parseInputHelper (l:ls) fields passports
            | null l =  parseInputHelper ls [] (passports ++ [Map.fromList fields])
            | otherwise = parseInputHelper ls (fields ++ fieldLineParse l) passports
        fieldLineParse :: String -> [(String, String)]
        fieldLineParse line = [fieldParse word | word <- words line]
        fieldParse :: String -> (String, String)
        fieldParse s = (head seg, (head . tail) seg)
            where seg = splitString s ':'

isValidPassport :: Passport -> [(String, String -> Bool)] -> Bool
isValidPassport passport rules = all (\pair -> Map.member (fst pair) passport && snd pair (passport Map.! fst pair)) rules

isValidYear :: Int -> Int -> String -> Bool
isValidYear lower upper s = case readMaybe s of
                            Nothing -> False
                            Just year -> lower <= year && year <= upper

isValidHeight :: String -> Bool
isValidHeight s = isValidHelperValue s ""
    where
        isValidHelperValue :: String -> String -> Bool
        isValidHelperValue "" _ = False
        isValidHelperValue (x:xs) numString
            | isDigit x = isValidHelperValue xs (numString ++ [x])
            | otherwise = not (null numString) && isValidHelperUnits (x:xs) (read numString)
        isValidHelperUnits :: String -> Int -> Bool
        isValidHelperUnits "cm" value = value >= 150 && value <= 193
        isValidHelperUnits "in" value = value >= 59 && value <= 76
        isValidHelperUnits _ _ = False

isValidHexColor :: String -> Bool
isValidHexColor s = head s == '#' && all isHex (tail s)

isHex :: Char -> Bool
isHex ch = isDigit ch || ('a' <= ch && ch <= 'f')

isValidEyeColor :: String -> Bool
isValidEyeColor s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValidPassportId :: String -> Bool
isValidPassportId s = length s == 9 && all isDigit s

main :: IO()
main = do
    input <- readFile "input04.txt"
    putStrLn "Advent of Code 2020 4.2"

    let passports = parseInput input
        rules = [ ("byr", isValidYear 1920 2002)
                , ("iyr", isValidYear 2010 2020)
                , ("eyr", isValidYear 2020 2030)
                , ("hgt", isValidHeight)
                , ("hcl", isValidHexColor)
                , ("ecl", isValidEyeColor)
                , ("pid", isValidPassportId ) ]
    let result = length [passport | passport <- passports, isValidPassport passport rules]

    print result

splitString :: String -> Char -> [String]
splitString s splitChar = splitStringHelper s "" []
    where
        splitStringHelper :: [Char] -> [Char] -> [[Char]] -> [[Char]]
        splitStringHelper [] word words = words ++ [word]
        splitStringHelper (ch:chs) word words
            | ch == splitChar = splitStringHelper chs "" (words ++ [word])
            | otherwise = splitStringHelper chs (word ++ [ch]) words