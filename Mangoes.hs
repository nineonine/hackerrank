module Mangoes where

import Control.Monad
import Data.List (sort)

-- Num of Guests -> Num of Mangoes -> [Appetites] -> [Happiness factors]
maxNumOfGuests :: Int -> Int -> [Int] -> [Int] -> Int
maxNumOfGuests _ _ [] _  = 0
maxNumOfGuests _ _ _ []  = 0
maxNumOfGuests n m as hs = maxPossible consumedMangos 0 0
                           where
                           eatMango = ( \ k a h -> a + (k-1) * h ) n
                           consumedMangos = sort $ zipWith eatMango as hs
                           maxPossible []     _   num = num
                           maxPossible (x:xs) acc num
                                | x + acc > m  = num
                                | x + acc == m = num + 1
                                | otherwise    = maxPossible xs (x+acc) (num + 1);

main :: IO ()
main = do
    let readFromInput = fmap ( map read . words ) getLine
    [n, m] <- readFromInput :: IO [Int]
    [as, hs] <- replicateM 2 readFromInput :: IO [[Int]]
    print $ maxNumOfGuests n m as hs
