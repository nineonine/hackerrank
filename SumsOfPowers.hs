module SumsOfPowers where

import Control.Monad (replicateM)

sumofpowers x n = go 1 x n
    where
    go m 0 n = 1;
    go m x n | m^n > x = 0;
    go m x n = go (m + 1) (x - m^n) n + go (m + 1) x n

main :: IO ()
main = do
    [x,n] <- replicateM 2 (fmap read getLine)
    print $ sumofpowers x n
