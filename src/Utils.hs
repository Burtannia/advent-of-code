module Utils where

import           Data.Char (toLower, toUpper)

ppPuzzle :: Show a => Int -> a -> IO ()
ppPuzzle n x = putStrLn $ "Puzzle " ++ show n ++ ": " ++ show x

ppDay :: (Show a, Show b) => Int -> String -> a -> b -> IO ()
ppDay year day x y = do
    putStrLn $ "-- " ++ day ++ " December " ++ show year ++ " --"
    ppPuzzle 1 x
    ppPuzzle 2 y

ppDay2021 :: (Show a, Show b) => String -> a -> b -> IO ()
ppDay2021 = ppDay 2021

upperFirst :: String -> String
upperFirst ""     = ""
upperFirst (c:cs) = toUpper c : cs
