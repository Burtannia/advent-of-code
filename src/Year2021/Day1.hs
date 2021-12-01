{-# LANGUAGE ScopedTypeVariables #-}

module Year2021.Day1 where

import           System.IO.Strict (readFile)

import           Data.Foldable    (foldl')
import           Prelude          hiding (readFile)

main :: IO ()
main = do
    putStrLn "-- 1st December 2021 --"

    ns :: [Int] <- map read . lines <$> readFile "input/2021/day1.txt"

    let puzzle1 = countIncreases ns
    putStrLn $ "Puzzle 1: " ++ show puzzle1

    let puzzle2 = countIncreases $ slidingWindow ns
    putStrLn $ "Puzzle 2: " ++ show puzzle2

countIncreases :: [Int] -> Int
countIncreases ns = length
    $ filter (== True)
    $ zipWith (<) (head ns : ns) ns
    -- OR
    -- snd $ foldl' (\(prev, count) n ->
    --     (n, if n > prev then count + 1 else count)) (head ns, 0) ns

slidingWindow :: [Int] -> [Int]
slidingWindow ns = drop 2
    $ zipWith (+) ns
    $ zipWith (+) (0:ns) (0:0:ns)
