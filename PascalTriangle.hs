module PascalTriangle where

import qualified Data.List as List
import qualified Data.Char as Char

mkRow :: Int -> [Int]
mkRow n = take (n+1) $ map (choose n) [0..] where choose n k = product [k+1..n] `div` product [1..n-k]

mkTriangle :: Int -> [[Int]]
mkTriangle n = take n $ map mkRow [0..]

printRow :: [Int] -> String
printRow [1] = "1"
printRow xs  = List.unwords $ map show xs

printTriangle :: [[Int]] -> IO ()
printTriangle = mapM_ (putStrLn . printRow)

main :: IO ()
main = do
    n <- fmap read getLine
    printTriangle $ mkTriangle n
