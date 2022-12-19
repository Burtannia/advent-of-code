{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Year2021.Day4 where

import           Control.Arrow                 (Arrow (second, (&&&)))
import           Control.Monad                 (join, void)
import           Data.Char                     (digitToInt)
import           Data.List                     (all, filter, find, foldl',
                                                iterate, map, or, sum,
                                                transpose, unfoldr, (++))
import           Data.Maybe                    (fromJust, isJust, listToMaybe,
                                                mapMaybe)
import           Prelude                       hiding (readFile)
import           System.IO.Strict              (readFile)
import           Text.ParserCombinators.Parsec (GenParser, char, digit, eof,
                                                many, many1, newline, oneOf,
                                                parse, sepBy, sepEndBy)
import           Utils                         (ppDay2021)

type Row = [(Int, Bool)]
type Board = [Row]

type ToCall = [Int]
type Called = [Int]

data Bingo = Bingo ToCall Called [Board]
    deriving Show

type Parser a = GenParser Char () a

whiteSpace :: Parser ()
whiteSpace = void $ many $ oneOf " \t"

parseNumber :: Parser Int
parseNumber = foldl' (\a n -> a * 10 + digitToInt n) 0 <$> many1 digit

parseNumbers :: Parser [Int]
parseNumbers = sepEndBy parseNumber (char ',') <* newline

parseRow :: Parser [(Int, Bool)]
parseRow = many1 ((, False) <$> (whiteSpace *> parseNumber))

parseBoard :: Parser Board
parseBoard = sepEndBy parseRow newline

parseBingo :: Parser Bingo
parseBingo = Bingo
    <$> parseNumbers
    <*> pure []
    <*  newline
    <*> sepBy parseBoard newline
    <*  eof

callNumber :: Int -> Board -> Board
callNumber n = map $ map $ \(i,b) -> if n == i then (i, True) else (i,b)

hasWon :: Board -> Bool
hasWon b = or $ map hasWonRow b ++ map hasWonRow (transpose b)
    where
        hasWonRow = all snd

getWinner :: Bingo -> Maybe (Int, Board)
getWinner (Bingo _ cs bs) = sequence (head cs, find hasWon bs)

playRound :: Bingo -> Bingo
playRound (Bingo [] _ _)       = error "No numbers left"
playRound (Bingo (n:ns) cs bs) = Bingo ns (n:cs) $ map (callNumber n) bs

outOfNumbers :: Bingo -> Bool
outOfNumbers (Bingo xs _ _) = null xs

allRounds :: Bingo -> [Bingo]
allRounds = unfoldr $ \b ->
    if outOfNumbers b
        then Nothing
        else Just $ (id &&& playRound) b

winners :: Bingo -> [(Int, Board)]
winners = mapMaybe getWinner . allRounds

sumUnmarked :: Board -> Int
sumUnmarked rs = sum [n | (n, marked) <- concat rs, not marked]

answer :: ([(Int, Board)] -> (Int, Board)) -> Bingo -> Int
answer f = uncurry (*) . second sumUnmarked . f . winners

ppBoard :: Board -> IO ()
ppBoard = putStrLn . unlines . map show

ppBingo :: Bingo -> IO ()
ppBingo (Bingo ns _ bs) = do
    putStr "Numbers: "
    print ns
    putStrLn ""
    putStrLn "Boards"
    mapM_ ppBoard bs

day4 :: IO ()
day4 = do
    (Right bingo) <- parse parseBingo "" <$> readFile "input/2021/day4.txt"
    let result1 = answer head bingo
    let result2 = last $ winners bingo
    print result2
    ppDay2021 "4th" result1 ""
