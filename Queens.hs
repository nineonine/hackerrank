module Queens where

board :: Int -> [(Int,Int)]
board 0 = []
board n = [ (x,y) | x <- [1..n] , y <- [1..n] ]



main :: IO ()
main = putStrLn "!"
