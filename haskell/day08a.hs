-- Advent of Code 2020
-- Day 8 Part 1

module Day08a where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data InstructionType = NoOperation | Jump | Accumulate deriving Show
data Instruction = Instruction { instructionType :: InstructionType, value :: Int } deriving Show

parseInstructions :: String -> Map.Map Int Instruction
parseInstructions s = parseInstructionsHelper (lines s) Map.empty 0
    where
        parseInstructionsHelper :: [String] -> Map.Map Int Instruction -> Int -> Map.Map Int Instruction
        parseInstructionsHelper [] instructions _ = instructions
        parseInstructionsHelper (l:ls) instructions lineNumber = parseInstructionsHelper ls (Map.insert lineNumber (parseInstruction l) instructions) (lineNumber + 1)

parseInstruction :: String -> Instruction
parseInstruction s =
    let pieces = words s
        instructionType = case head pieces of
            "nop" -> NoOperation
            "jmp" -> Jump
            "acc" -> Accumulate
            _ -> error ("Unrecognized error: " ++ head pieces)
        value = if head (head (tail pieces)) == '+' then read (tail (head (tail pieces))) else read (head (tail pieces))
    in Instruction { instructionType = instructionType, value = value }

findLoop :: Map.Map Int Instruction -> Int
findLoop instructions = findLoopIterator 0 0 Set.empty 
    where
        findLoopIterator line accumulator visited
                | line `Set.member` visited = accumulator
                | otherwise = case instructionType (instructions Map.! line) of
                    NoOperation -> findLoopIterator (line + 1) accumulator (Set.insert line visited)
                    Jump -> findLoopIterator (line + value ( instructions Map.! line )) accumulator (Set.insert line visited)
                    Accumulate -> findLoopIterator (line + 1) (accumulator + value ( instructions Map.! line )) (Set.insert line visited)


main :: IO()
main = do
    input <- readFile "input08.txt" 
    putStrLn "Advent of Code 2020 8.1"

    let instructions = parseInstructions input
    let result = findLoop instructions

    print result
