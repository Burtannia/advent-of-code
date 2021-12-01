{-# LANGUAGE ScopedTypeVariables #-}

module Year2021.Day1 where

import           System.IO.Strict (readFile)

import           Data.Foldable    (foldl')
import           Prelude          hiding (readFile)

main :: IO ()
main = puzzle1 >>= print

puzzle1 :: IO Int
puzzle1 = do
    ns :: [Int] <- map read . lines <$> readFile "input/2021/day1.txt"
    return $ length $ filter (== True) $ zipWith (<) (head ns : ns) ns
    -- OR
    -- return $ snd $ foldl' (\(prev, count) n ->
    --     (n, if n > prev then count + 1 else count)) (head ns, 0) ns

puzzle2 :: IO Int
puzzle2 = return 1
