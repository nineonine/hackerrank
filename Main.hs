module Main where

solve :: Double -> Double
solve x = sum . take 10 $ 1 : x : gen 2 x
          where
          factorial x = if x == 0 then 1 else x * factorial (x - 1)
          gen n val = val^n / (fromIntegral $ factorial n) : gen (n + 1) val


main :: IO ()
main = getContents >>= mapM_ print. map solve. map (read::String->Double). tail. words
