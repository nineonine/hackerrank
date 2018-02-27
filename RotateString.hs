module RotateString where

import Control.Monad (replicateM)

-- Enter your code here. Read input from STDIN. Print output to STDOUT

rotations :: String -> [String]
rotations s = let next = take (length s) . tail $ cycle s
              in take (length s) $ next : rotations next

main :: IO ()
main = do
    n  <- fmap read getLine
    xs <- replicateM n getLine
    mapM_ ((putStrLn . unwords) . rotations) xs
