{-# LANGUAGE ScopedTypeVariables #-}

module Year2021.Day3 where

import           Control.Arrow  ((&&&))
import           Data.Bifunctor (bimap)
import           Data.Foldable  (foldl')
import           Data.List      (transpose, unzip)
import           Utils          (parseBin, ppDay2021)

mostLeast :: String -> (Char, Char)
mostLeast xs = if os >= zs then ('1','0') else ('0','1')
    where
        os = length $ filter (== '1') xs
        zs = length xs - os

parseMul :: (String, String) -> Int
parseMul = uncurry (*) . bimap parseBin parseBin

data LengthList a = LL Int [a]

empty :: LengthList a
empty = LL 0 []

(.:) :: a -> LengthList a -> LengthList a
(.:) x (LL n xs) = LL (n + 1) (x : xs)

splitTails :: [String] -> (LengthList String, LengthList String)
splitTails = foldl' f (empty, empty)
    where
        f (os, zs) ('0' : xs) = (os, xs .: zs)
        f (os, zs) ('1' : xs) = (xs .: os, zs)
        f (os, zs) _          = (os, zs)

recursiveSplit :: (Int -> Int -> Bool) -> [String] -> String
recursiveSplit _ [x]  = x
recursiveSplit cmp xs = y : recursiveSplit cmp ys
    where
        (y, ys) = if n `cmp` n' then ('1', os) else ('0', zs)
        (LL n os, LL n' zs) = splitTails xs

day3 :: IO ()
day3 = do
    xs :: [String] <- lines <$> readFile "input/2021/day3.txt"
    let result1 = parseMul $ unzip $ map mostLeast $ transpose xs
        result2 = parseMul $ (recursiveSplit (>=) &&& recursiveSplit (<)) xs
    ppDay2021 "3rd" result1 result2
