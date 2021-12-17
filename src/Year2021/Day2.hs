{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2021.Day2 where

import           Data.Foldable    (foldl')
import           Prelude          hiding (readFile)
import           System.IO.Strict (readFile)
import           Utils            (ppDay2021, upperFirst)

data BasicSub = BasicSub
    { subHoriz :: Int
    , subDepth :: Int
    }

data AdvancedSub = AdvancedSub
    { advHoriz :: Int
    , advDepth :: Int
    , advAim   :: Int
    }

data Move = Forward Int | Down Int | Up Int
    deriving (Show, Read)

class Submarine a where
    move :: Move -> a -> a

instance Submarine BasicSub where
    move (Up x) s      = s { subDepth = subDepth s - x }
    move (Down x) s    = s { subDepth = subDepth s + x }
    move (Forward x) s = s { subHoriz = subHoriz s + x }

instance Submarine AdvancedSub where
    move (Up x) s   = s { advAim = advAim s - x }
    move (Down x) s = s { advAim = advAim s + x }
    move (Forward x) s@AdvancedSub {..} =
        s { advHoriz = advHoriz + x, advDepth = advDepth + (advAim * x) }

day2 :: IO ()
day2 = do
    ms :: [Move] <- map (read . upperFirst) . lines <$> readFile "input/2021/day2.txt"
    let BasicSub {..} = foldl' (flip move) (BasicSub 0 0) ms
    let AdvancedSub {..} = foldl' (flip move) (AdvancedSub 0 0 0) ms
    ppDay2021 "2nd" (subHoriz * subDepth) (advHoriz * advDepth)
