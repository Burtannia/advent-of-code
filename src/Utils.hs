module Utils where

import           Data.Char        (digitToInt, toUpper)
import           Numeric          (readInt)
import qualified System.IO.Strict as Strict

ppPuzzle :: Show a => Int -> a -> IO ()
ppPuzzle n x = putStrLn $ "Puzzle " ++ show n ++ ": " ++ show x

ppDay :: (Show a, Show b) => Int -> String -> a -> b -> IO ()
ppDay year day x y = do
    putStrLn $ "-- " ++ day ++ " December " ++ show year ++ " --"
    ppPuzzle 1 x
    ppPuzzle 2 y

ppDay2021 :: (Show a, Show b) => String -> a -> b -> IO ()
ppDay2021 = ppDay 2021

ppDay2022 :: (Show a, Show b) => String -> a -> b -> IO ()
ppDay2022 = ppDay 2021

upperFirst :: String -> String
upperFirst ""     = ""
upperFirst (c:cs) = toUpper c : cs

parseBin :: String -> Int
parseBin = fst . head . readInt 2 (`elem` "01") digitToInt

daySuffix :: Int -> String
daySuffix day
    | day `mod` 10 == 1 = "st"
    | day `mod` 10 == 2 = "nd"
    | day `mod` 10 == 3 = "rd"
    | otherwise = "th"

type Puzzle a = [String] -> a

mkDay :: (Show a, Show b) => Int -> Int -> Puzzle a -> Puzzle b -> IO ()
mkDay year day px py = do
    input <- lines <$> Strict.readFile fileName
    ppDay year dayString (px input) (py input)
    where
        fileName = "input/" <> show year <> "/day" <> show day <> ".txt"
        dayString = show day <> daySuffix day

mkDay2022 :: (Show a, Show b) => Int -> Puzzle a -> Puzzle b -> IO ()
mkDay2022 = mkDay 2022
