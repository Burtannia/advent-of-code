module Year2022.Day1 where

import           Data.List        (sort)
import           Data.List.Split  (wordsBy)
import qualified System.IO.Strict as Strict (readFile)
import           Text.Read        (readMaybe)
import           Utils            (ppDay2022)

day1 :: IO ()
day1 = do
    input <- lines <$> Strict.readFile "input/2022/day1.txt"
    ppDay2022 "1st" (puzzle1 input) (puzzle2 input)

type Elf = [String]

splitElves :: [String] -> [Elf]
splitElves = wordsBy (== "")

sumElf :: Elf -> Int
sumElf = sum . map read

mostCalorificElf :: [Elf] -> Int
mostCalorificElf = maximum . map sumElf

puzzle1 :: [String] -> Int
puzzle1 = mostCalorificElf . splitElves

top3Elves :: [Elf] -> [Int]
top3Elves = take 3 . reverse . sort . map sumElf

puzzle2 :: [String] -> Int
puzzle2 = sum . top3Elves . splitElves
