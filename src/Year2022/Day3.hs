module Year2022.Day3 where

import           Data.Char  (isUpper, ord)
import           Data.Maybe (catMaybes)
import           Utils      (mkDay2022)

day3 :: IO ()
day3 = mkDay2022 3 puzzle1 puzzle2

halves :: [a] -> ([a], [a])
halves xs = splitAt (length xs `div` 2) xs

overlaps :: Eq a => [a] -> [a] -> [a]
overlaps [] _ = error "Empty list"
overlaps (x:xs) ys = overlaps' x ys
    where
        overlaps' _ [] = overlaps xs ys
        overlaps' x (z:zs)
            | x == z = x : overlaps xs ys
            | otherwise = overlaps' x zs

charValue :: Char -> Int
charValue c = fromEnum c - n
    where
        n = if isUpper c then 38 else 96

puzzle1 :: [String] -> Int
puzzle1 = sum . map (charValue . head . uncurry overlaps . halves)

tripleOverlap :: Eq a => [a] -> [a] -> [a] -> a
tripleOverlap xs ys zs = head $ overlaps (overlaps xs ys) zs

threes :: [a] -> [[a]]
threes xs
    | length xs <= 3 = [xs]
    | otherwise = ys : threes zs
        where
            (ys, zs) = splitAt 3 xs

puzzle2 :: [String] -> Int
puzzle2 backpacks = sum [ charValue $ tripleOverlap xs ys zs | [xs, ys, zs] <- threes backpacks ]
