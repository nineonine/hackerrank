module JumpingBunnies where

import Data.List (foldl1')

main :: IO ()
main = do
    bunnies <- getLine >> fmap (map (read :: String -> Integer) . words) getLine
    print $ foldl1' lcm bunnies
