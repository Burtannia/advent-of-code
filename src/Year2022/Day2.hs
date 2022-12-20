module Year2022.Day2 where

import           Data.Function    (on)
import           Data.List        (group, maximumBy, sortBy)
import qualified System.IO.Strict as Strict
import           Utils            (mkDay2022, ppDay2022)

day2 :: IO ()
day2 = mkDay2022 2 puzzle1 puzzle2

data Move = Rock | Paper | Scissors
    deriving (Show, Eq)

data Outcome = Win | Lose | Draw
    deriving (Show, Eq)

movePoints :: Move -> Int
movePoints Rock     = 1
movePoints Paper    = 2
movePoints Scissors = 3

outcomePoints :: Outcome -> Int
outcomePoints Win  = 6
outcomePoints Draw = 3
outcomePoints Lose = 0

readMove :: Char -> Move
readMove 'A' = Rock
readMove 'B' = Paper
readMove 'C' = Scissors
readMove 'X' = Rock
readMove 'Y' = Paper
readMove 'Z' = Scissors
readMove _   = error "Invalid character"

-- Get outcome for move A vs move B
calcOutcome :: Move -> Move -> Outcome
calcOutcome Rock Scissors = Win
calcOutcome Scissors Paper = Win
calcOutcome Paper Rock = Win
calcOutcome x y
    | x == y = Draw
    | otherwise = Lose

-- Get score for move A vs move B
calcScore :: Move -> Move -> Int
calcScore x y = movePoints x + outcomePoints (calcOutcome x y)

readGame :: String -> (Move, Move)
readGame (x : ' ' : y : _) = (readMove x, readMove y)
readGame _                 = error "Invalid input"

puzzle1 :: [String] -> Int
puzzle1 = sum . map ((uncurry . flip) calcScore . readGame)

readOutcome :: Char -> Outcome
readOutcome 'X' = Lose
readOutcome 'Y' = Draw
readOutcome 'Z' = Win
readOutcome _   = error "Invalid input"

readGame2 :: String -> (Move, Outcome)
readGame2 (x : ' ' : y : _) = (readMove x, readOutcome y)
readGame2 _                 = error "Invalid input"

moveForOutcome :: Move -> Outcome -> Move
moveForOutcome x Draw        = x
moveForOutcome Rock Win      = Paper
moveForOutcome Scissors Lose = Paper
moveForOutcome Paper Win     = Scissors
moveForOutcome Rock Lose     = Scissors
moveForOutcome _ _           = Rock

calcScore2 :: Move -> Outcome -> Int
calcScore2 oppMove outcome = calcScore myMove oppMove
    where
        myMove = moveForOutcome oppMove outcome

puzzle2 :: [String] -> Int
puzzle2 = sum . map (uncurry calcScore2 . readGame2)
