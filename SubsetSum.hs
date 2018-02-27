module SubsetSum where

import Data.List
import Data.Function
import Control.Monad

getFromIO f = fmap f getLine

subsetSum :: [Int] -> Int -> Int
subsetSum xs v
    | last xs >= v = 1
    | otherwise    = go sorted
                     where
                     sorted = tail . sortBy (compare `on` length) $ subsequences xs
                     go :: [[Int]] -> Int
                     go [] = -1
                     go (y:ys)
                         | sum y >= v = length y
                         | otherwise  = go ys


main :: IO ()
main = do
    listA <- getLine >> getFromIO ( map (read :: String -> Int) . words )
    numOfCases <- getFromIO (read :: String -> Int)
    cs <- replicateM numOfCases $ getFromIO (read :: String -> Int)
    forM_ cs ( putStrLn . show . subsetSum listA )
